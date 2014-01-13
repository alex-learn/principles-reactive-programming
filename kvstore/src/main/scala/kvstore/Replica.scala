package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import scala.language.postfixOps
import scala.language.implicitConversions

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class CheckAck(key: String, id: Long)
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply


  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[Key, Value]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[Replica, Replicator]
  // the current set of replicators
  var replicators = Set.empty[Replicator]

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  //  Aditional logic ...
  
  type Id = Long
  type Key = String
  type Value = String
  type Requester = ActorRef
  type Replica = ActorRef
  type Replicator = ActorRef

  val persistence: ActorRef = context.actorOf(persistenceProps)
  private[kvstore] var expectedSeq = 0L
  private[kvstore] var persistenceAcks = Map.empty[Id, (Requester, Persist)] 

  //  Main replica behaviour

  val leader: Receive = {

    case Insert(key, value, id) => 
      kv += key -> value
      replicate(key, value, id)

    case Get(key, id) => 
      sender ! GetResult(key, kv get key, id)

    case Remove(key, id) => 
      kv -= key
      replicate(key, None, id)

    case Persisted(key, id) => 
      Ack persistence(key, id)

    case Replicated(key, id) => 
      Ack replication(key, id, sender)
    
    case CheckAck(key, id) => 
      Ack checkIfFailed(key, id)

    case Replicas(replicas) => {
      val added = replicas -- secondaries.keySet - self
      val removed = secondaries.keySet -- replicas
      removed.foreach { removed =>
        val replicator = secondaries(removed)
        Ack remove replicator
        context stop replicator
        secondaries -= removed
      }
      added.foreach { added =>
        val replicator = context.actorOf(Replicator.props(added))
        secondaries += added -> replicator
        for {(key,value) <- kv} 
          replicator ! Replicate(key, value, secondaries.size)
      }
    }

  }

  val replica: Receive = {

    case Snapshot(key, valueOption, seq) => 
      if(seq == expectedSeq){
        expectedSeq += 1
        update(key, valueOption)
        persist(key, valueOption, seq)
      } else if (seq < expectedSeq)
        sender ! SnapshotAck(key, seq)

    case Get(key, id) => 
      sender ! GetResult(key, kv.get(key), id)

    case Persisted(key, id) =>
      val (requester,_) = persistenceAcks(id)
      requester ! SnapshotAck(key, id)
      persistenceAcks -= id

  }


  def replicate(key: Key, valueOption: Option[Value], id: Id) {
    secondaries.values.foreach { replicator =>
      Ack registerReplication(key, id, replicator, sender)
      replicator ! Replicate(key, valueOption, id)
    }
    Ack registerPersistence(key, id, sender)
    persist(key, valueOption, id)
    context.system.scheduler.scheduleOnce(1 second, self, CheckAck(key, id))
  }

  def persist( key: Key, valueOption: Option[Value], id: Id) {
    val p = Persist(key, valueOption, id)
    persistenceAcks += id -> (sender, p)
    persistence ! p
  }

  def update(key: Key, valueOption: Option[Value]) {
    if(valueOption.isDefined)
      kv += key -> valueOption.get
    else
      kv -= key
  }

  override def preStart() : Unit= {
    arbiter ! Join
    context.system.scheduler.schedule(0 milliseconds, 100 milliseconds){
      persistenceAcks.foreach {
        case (id, (_, p)) => persistence ! p
      }
    }
  }

  case class Ack(
    key: Key,
    id: Id,
    pendingReplicators: Set[Replicator] = Set(),
    requester: Requester,
    persistenceAcked: Boolean = false) {
     def acked : Boolean = persistenceAcked && pendingReplicators.isEmpty
  }

  object Ack {
    var pending = Map.empty[(Key, Id), Ack]

    def persistence(key: Key, id: Id) {
      for (ack <- pending.get(key,id)) 
        checkIfDone(ack.copy(persistenceAcked=true))
    }

    def replication(key: Key, id: Id, replicator: Replicator) {
      for (ack <- pending.get(key,id)) 
        checkIfDone(ack.copy(pendingReplicators = ack.pendingReplicators - replicator))
    }

    private def checkIfDone(ack: Ack) {
      if (ack.acked) {
        ack.requester ! OperationAck(ack.id)
        pending -= ((ack.key, ack.id))
      } else 
        pending += ((ack.key, ack.id)) -> ack
    }

    def checkIfFailed(key: Key, id: Long) {
      for (ack <- pending.get((key,id))){
        ack.requester ! OperationFailed(id)
        pending -= ((key, id))
      }
    }

    def registerPersistence(key: Key, id: Long, requester: Requester) {
      pending += ((key,id)) -> get(key, id, requester).copy(persistenceAcked = true)
    }

    def registerReplication(key: Key, id: Long, replicator: Replicator, requester: Requester) {
      val existing = get(key, id, requester)
      pending += ((key, id)) -> existing.copy(pendingReplicators = existing.pendingReplicators + replicator)
    }

    def get(key: Key, id: Long, requester: Requester): Ack = {
      pending.getOrElse((key,id), Ack(key = key, id = id, requester = requester))
    }

    def remove(replicator: Replicator) {
      pending.values.foreach { ack =>
        checkIfDone(ack.copy(pendingReplicators = ack.pendingReplicators - replicator))
      }
    }
  }

  //  Helpers

  implicit def fromValueToOpt(from: Value): Option[Value] = Option(from)

}