{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Profile.Live.Server.Events.Model
Description : DB representation of an eventlog event
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Events.Model where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Data.Word 
import Database.Persist
import Database.Persist.TH 

import GHC.RTS.Events 

type EventTypeNum = Word16
type EventTypeDesc = String
type EventTypeSize = Word16
type BlockSize = Word64 -- larges of used in eventlog types
type Capset   = Word32
type PID = Word32
type StringId = Word32
type ParConjDynId = Word64
type ParConjStaticId = StringId
type SparkId = Word32
type FutureId = Word64
type PerfEventTypeNum = Word32
type MessageTagImpl = Word 
type KernelThreadIdImpl = Word64
type CapsetTypeImpl = Word 

share [mkPersist sqlSettings
     , mkDeleteCascade sqlSettings
     , mkMigrate "migrateAll"] [persistLowerCase|
EventLogImpl

EventTypeImpl
  eventLog EventLogImplId
  num EventTypeNum
  desc EventTypeDesc
  size EventTypeSize Maybe 
  deriving Show Eq 

EventImpl
  eventLog EventLogImplId
  time Timestamp
  spec EventInfoImpl
  cap Int Maybe 
  deriving Show 

ThreadStopStatusImpl 
  num Word 
  threadId ThreadId Maybe 
  deriving Show 

EventInfoImpl
  type EventTypeNum -- decoder needs this field
  -- EventBlock
  endTime Timestamp Maybe
  cap Int Maybe -- Also TaskCreate, TaskMigrate,
    -- CapCreate, CapDelete, CapDisable, 
    -- CapEnable, CapsetAssignCap, CapsetRemoveCap
  blockSize BlockSize Maybe -- Also GCStatsGHC
  -- UnknownEvent
  ref EventTypeNum Maybe 
  -- Startup 
  nCaps Int Maybe 
  -- Shutdown is not presented
  -- CreateThread, RunThread, StopThread,
  --  ThreadRunnable, MigrateThread, WakeupThread,
  --  ThreadLabel, AssignThreadToProcess
  thread ThreadId Maybe 
  -- StopThread
  status ThreadStopStatusImpl Maybe
  -- MigrateThread, TaskMigrate
  newCap Int Maybe 
  -- WakeupThread
  otherCap Int Maybe 
  -- ThreadLabel
  threadlabel String Maybe 
  -- CreateSparkThread
  sparkThread ThreadId Maybe 
  -- SparkCounters
  sparksCreated Word64 Maybe 
  sparksDud Word64 Maybe 
  sparksOverflowed Word64 Maybe 
  sparksConverted Word64 Maybe 
  sparksFizzled Word64 Maybe 
  sparksGCd Word64 Maybe
  sparksRemaining Word64 Maybe 
  -- SparkCreate
  -- SparkDud
  -- SparkOverflow
  -- SparkRun
  -- SparkSteal
  victimCap Int Maybe
  -- SparkFizzle
  -- SparkGC
  -- TaskCreate, TaskMigrate, TaskDelete
  taskId TaskId Maybe
  -- TaskCreate, PerfCounter, PerfTracepoint
  tid KernelThreadIdImpl Maybe 
  -- RequestSeqGC
  -- RequestParGC
  -- StartGC
  -- GCWork
  -- GCIdle
  -- GCDone
  -- EndGC
  -- GlobalSyncGC
  -- GCStatsGHC, HeapAllocated, HeapSize, HeapLive,
  --  HeapInfoGHC
  heapCapset Capset Maybe  
  -- GCStatsGHC
  gen Int Maybe 
  copied Word64 Maybe 
  slop Word64 Maybe 
  frag Word64 Maybe 
  parNThreads Int Maybe 
  parMaxCopied Word64 Maybe 
  parTotCopied Word64 Maybe 
  -- HeapAllocated
  allocBytes Word64 Maybe 
  -- HeapSize
  sizeBytes Word64 Maybe
  -- HeapLive
  liveBytes Word64 Maybe 
  -- HeapInfoGHC
  gens Int Maybe 
  maxHeapSize Word64 Maybe 
  allocAreaSize Word64 Maybe 
  mblockSize Word64 Maybe
  -- CapCreate
  -- CapDelete
  -- CapDisable
  -- CapEnable
  -- CapsetCreate, CapsetDelete, CapsetAssignCap, CapsetRemoveCap, RtsIdentifier, ProgramArgs, ProgramEnv, OsProcessPid, OsProcessParentPid, WallClockTime
  capset Capset Maybe 
  -- CapsetCreate
  capsetType CapsetTypeImpl Maybe 
  -- CapsetDelete
  -- CapsetRemoveCap
  -- RtsIdentifier
  rtsident String Maybe 
  -- ProgramArgs
  args [String] Maybe 
  -- ProgramEnv
  env [String] Maybe 
  -- OsProcessPid
  pid PID Maybe 
  -- OsProcessParentPid
  ppid PID Maybe 
  -- WallClockTime
  sec Word64 Maybe 
  nsec Word32 Maybe 
  -- Message, UserMessage
  msg String Maybe 
  -- UserMarker
  markername String Maybe 
  -- Version
  version String Maybe 
  -- ProgramInvocation
  commandline String Maybe 
  -- CreateMachine, KillMachine
  machine MachineId Maybe
  -- CreateMachine 
  realtime Timestamp Maybe 
  -- CreateProcess, KillProcess, AssignThreadToProcess
  process ProcessId Maybe 
  -- EdenStartReceive
  -- EdenEndReceive
  -- SendMessage, ReceiveMessage, SendReceiveLocalMessage
  mesTag MessageTagImpl Maybe 
  senderProcess ProcessId Maybe 
  senderThread ThreadId Maybe 
  receiverMachine MachineId Maybe
  receiverProcess ProcessId Maybe 
  receiverInport PortId Maybe
  -- ReceiveMessage 
  senderMachine MachineId Maybe 
  messageSize MessageSize Maybe
  -- SendReceiveLocalMessage
  -- InternString
  str String Maybe 
  sId StringId Maybe 
  -- MerStartParConjunction, MerEndParConjunction, MerEndParConjunct, MerCreateSpark
  dynId ParConjDynId Maybe 
  -- MerStartParConjunction
  staticId ParConjStaticId Maybe 
  -- MerCreateSpark
  sparkId SparkId Maybe 
  -- MerFutureCreate, MerFutureWaitNosuspend, MerFutureWaitSuspended, MerFutureSignal
  futureId FutureId Maybe 
  -- MerFutureCreate
  nameId StringId Maybe 
  -- MerFutureWaitNosuspend
  -- MerFutureWaitSuspended
  -- MerFutureSignal
  -- MerLookingForGlobalThread
  -- MerWorkStealing
  -- MerLookingForLocalSpark
  -- MerReleaseThread
  threadId ThreadId Maybe
  -- MerCapSleeping
  -- MerCallingMain
  -- PerfName, PerfCounter, PerfTracepoint
  perfNum PerfEventTypeNum Maybe 
  -- PerfName
  name String Maybe 
  -- PerfCounter
  period Word64 Maybe

  deriving Show 
|]

-- | Helper to decode from DB representation
fromThreadStopStatusImpl :: ThreadStopStatusImpl -> Maybe ThreadStopStatus
fromThreadStopStatusImpl ThreadStopStatusImpl{..} = case threadStopStatusImplNum of 
  0 -> Just NoStatus
  1 -> Just HeapOverflow
  2 -> Just StackOverflow
  3 -> Just ThreadYielding
  4 -> Just ThreadBlocked
  5 -> Just ThreadFinished
  6 -> Just ForeignCall
  7 -> Just BlockedOnMVar
  8 -> Just BlockedOnMVarRead
  9 -> Just BlockedOnBlackHole
  10 -> Just BlockedOnRead
  11 -> Just BlockedOnWrite
  12 -> Just BlockedOnDelay
  13 -> Just BlockedOnSTM
  14 -> Just BlockedOnDoProc
  15 -> Just BlockedOnCCall
  16 -> Just BlockedOnCCall_NoUnblockExc
  17 -> Just BlockedOnMsgThrowTo
  18 -> Just ThreadMigrating
  19 -> Just BlockedOnMsgGlobalise
  20 -> BlockedOnBlackHoleOwnedBy <$> threadStopStatusImplThreadId
  _ -> Nothing

-- | Helper to convert into DB representation
toThreadStopStatusImpl :: ThreadStopStatus -> ThreadStopStatusImpl 
toThreadStopStatusImpl st = case st of 
  NoStatus -> ThreadStopStatusImpl 0 Nothing
  HeapOverflow -> ThreadStopStatusImpl 1 Nothing
  StackOverflow -> ThreadStopStatusImpl 2 Nothing
  ThreadYielding -> ThreadStopStatusImpl 3 Nothing
  ThreadBlocked -> ThreadStopStatusImpl 4 Nothing
  ThreadFinished -> ThreadStopStatusImpl 5 Nothing
  ForeignCall -> ThreadStopStatusImpl 6 Nothing
  BlockedOnMVar -> ThreadStopStatusImpl 7 Nothing
  BlockedOnMVarRead -> ThreadStopStatusImpl 8 Nothing
  BlockedOnBlackHole -> ThreadStopStatusImpl 9 Nothing
  BlockedOnRead -> ThreadStopStatusImpl 10 Nothing
  BlockedOnWrite -> ThreadStopStatusImpl 11 Nothing
  BlockedOnDelay -> ThreadStopStatusImpl 12 Nothing
  BlockedOnSTM -> ThreadStopStatusImpl 13 Nothing
  BlockedOnDoProc -> ThreadStopStatusImpl 14 Nothing
  BlockedOnCCall -> ThreadStopStatusImpl 15 Nothing
  BlockedOnCCall_NoUnblockExc -> ThreadStopStatusImpl 16 Nothing
  BlockedOnMsgThrowTo -> ThreadStopStatusImpl 17 Nothing
  ThreadMigrating -> ThreadStopStatusImpl 18 Nothing
  BlockedOnMsgGlobalise -> ThreadStopStatusImpl 19 Nothing
  BlockedOnBlackHoleOwnedBy t -> ThreadStopStatusImpl 20 (Just t)

-- | Helper to convert from DB representation
fromMessageTagImpl :: MessageTagImpl -> Maybe MessageTag 
fromMessageTagImpl t = case t of 
  0 -> Just Ready
  1 -> Just NewPE
  2 -> Just PETIDS
  3 -> Just Finish
  4 -> Just FailPE
  5 -> Just RFork
  6 -> Just Connect
  7 -> Just DataMes
  8 -> Just Head
  9 -> Just Constr
  10 -> Just Part
  11 -> Just Terminate
  12 -> Just Packet
  _ -> Nothing 

-- | Helper to convert to DB representation
toMessageTagImpl :: MessageTag -> MessageTagImpl 
toMessageTagImpl t = case t of 
  Ready -> 0 
  NewPE -> 1 
  PETIDS -> 2 
  Finish -> 3 
  FailPE -> 4 
  RFork -> 5 
  Connect -> 6 
  DataMes -> 7 
  Head -> 8 
  Constr -> 9 
  Part -> 10 
  Terminate -> 11 
  Packet -> 12 

-- | Helper to convert from DB representation
fromCapsetTypeImpl :: CapsetTypeImpl -> Maybe CapsetType 
fromCapsetTypeImpl i = case i of 
  0 -> Just CapsetCustom
  1 -> Just CapsetOsProcess
  2 -> Just CapsetClockDomain
  3 -> Just CapsetUnknown
  _ -> Nothing

-- | Helper to convert to DB representation
toCapsetTypeImpl :: CapsetType -> CapsetTypeImpl
toCapsetTypeImpl c = case c of 
  CapsetCustom -> 0
  CapsetOsProcess -> 1
  CapsetClockDomain -> 2 
  CapsetUnknown -> 3

-- | Helper to convert to DB representation
toEventImpl :: EventLogImplId -> Event -> EventImpl 
toEventImpl i Event{..} = EventImpl {
    eventImplEventLog = i
  , eventImplTime = evTime 
  , eventImplSpec = toEventInfoImpl evSpec
  , eventImplCap = evCap
  }

-- | Helper to convert from DB representation
fromEventImpl :: EventImpl -> Maybe Event 
fromEventImpl EventImpl{..} = Event 
  <$> pure eventImplTime
  <*> fromEventInfoImpl eventImplSpec
  <*> pure eventImplCap

-- | Helper to convert to DB representation
toEventTypeImpl :: EventLogImplId -> EventType -> EventTypeImpl 
toEventTypeImpl i EventType{..} = EventTypeImpl {
    eventTypeImplEventLog = i
  , eventTypeImplNum = num 
  , eventTypeImplDesc = desc 
  , eventTypeImplSize = size 
  }

-- | Helper to convert from DB representation
fromEventTypeImpl :: EventTypeImpl -> EventType 
fromEventTypeImpl EventTypeImpl{..} = EventType {
    num = eventTypeImplNum 
  , desc = eventTypeImplDesc 
  , size = eventTypeImplSize 
  }

-- | Helper to create event info row without data
emptyEventInfoImpl :: EventTypeNum -> EventInfoImpl 
emptyEventInfoImpl t = EventInfoImpl {
    eventInfoImplType = t
  , eventInfoImplEndTime = Nothing
  , eventInfoImplCap = Nothing
  , eventInfoImplBlockSize = Nothing
  , eventInfoImplRef = Nothing
  , eventInfoImplNCaps = Nothing
  , eventInfoImplThread = Nothing
  , eventInfoImplStatus = Nothing
  , eventInfoImplNewCap = Nothing
  , eventInfoImplOtherCap = Nothing
  , eventInfoImplThreadlabel = Nothing
  , eventInfoImplSparkThread = Nothing
  , eventInfoImplSparksCreated = Nothing
  , eventInfoImplSparksDud = Nothing
  , eventInfoImplSparksOverflowed = Nothing
  , eventInfoImplSparksConverted = Nothing
  , eventInfoImplSparksFizzled = Nothing
  , eventInfoImplSparksGCd = Nothing
  , eventInfoImplSparksRemaining = Nothing
  , eventInfoImplVictimCap = Nothing
  , eventInfoImplTaskId = Nothing
  , eventInfoImplTid = Nothing
  , eventInfoImplHeapCapset = Nothing
  , eventInfoImplGen = Nothing
  , eventInfoImplCopied = Nothing
  , eventInfoImplSlop = Nothing
  , eventInfoImplFrag = Nothing
  , eventInfoImplParNThreads = Nothing
  , eventInfoImplParMaxCopied = Nothing
  , eventInfoImplParTotCopied = Nothing
  , eventInfoImplAllocBytes = Nothing 
  , eventInfoImplSizeBytes = Nothing
  , eventInfoImplLiveBytes = Nothing
  , eventInfoImplGens = Nothing
  , eventInfoImplMaxHeapSize = Nothing
  , eventInfoImplAllocAreaSize = Nothing
  , eventInfoImplMblockSize = Nothing
  , eventInfoImplCapset = Nothing
  , eventInfoImplCapsetType = Nothing
  , eventInfoImplRtsident = Nothing
  , eventInfoImplArgs = Nothing
  , eventInfoImplEnv = Nothing
  , eventInfoImplPid = Nothing
  , eventInfoImplPpid = Nothing
  , eventInfoImplSec = Nothing
  , eventInfoImplNsec = Nothing
  , eventInfoImplMsg = Nothing
  , eventInfoImplMarkername = Nothing
  , eventInfoImplVersion = Nothing
  , eventInfoImplCommandline = Nothing
  , eventInfoImplMachine = Nothing
  , eventInfoImplRealtime = Nothing
  , eventInfoImplProcess = Nothing
  , eventInfoImplMesTag = Nothing
  , eventInfoImplSenderProcess = Nothing
  , eventInfoImplSenderThread = Nothing
  , eventInfoImplReceiverMachine = Nothing
  , eventInfoImplReceiverProcess = Nothing
  , eventInfoImplReceiverInport = Nothing
  , eventInfoImplSenderMachine = Nothing
  , eventInfoImplMessageSize = Nothing
  , eventInfoImplStr = Nothing
  , eventInfoImplSId = Nothing
  , eventInfoImplDynId = Nothing
  , eventInfoImplStaticId = Nothing
  , eventInfoImplSparkId = Nothing
  , eventInfoImplFutureId = Nothing
  , eventInfoImplNameId = Nothing
  , eventInfoImplThreadId = Nothing
  , eventInfoImplPerfNum = Nothing
  , eventInfoImplName = Nothing
  , eventInfoImplPeriod = Nothing
  }

-- | Helper to convert to DB representation
toEventInfoImpl :: EventInfo -> EventInfoImpl 
toEventInfoImpl e = case e of 
  EventBlock{..} -> defImpl {
      eventInfoImplEndTime = Just end_time
    , eventInfoImplCap = Just cap 
    , eventInfoImplBlockSize = Just $ fromIntegral block_size
    }
  UnknownEvent{..} -> defImpl {
      eventInfoImplRef = Just ref
    }
  Startup {..} -> defImpl {
      eventInfoImplNCaps = Just n_caps
    }
  Shutdown -> defImpl
  CreateThread {..} -> defImpl {
      eventInfoImplThread = Just thread
    }
  RunThread {..} -> defImpl {
      eventInfoImplThread = Just thread
    }
  StopThread {..} -> defImpl {
      eventInfoImplThread = Just thread
    , eventInfoImplStatus = Just $ toThreadStopStatusImpl status
    }
  ThreadRunnable {..} -> defImpl {
      eventInfoImplThread = Just thread
    }
  MigrateThread {..} -> defImpl {
      eventInfoImplThread = Just thread
    , eventInfoImplNewCap = Just newCap
    }
  WakeupThread {..} -> defImpl {
      eventInfoImplThread = Just thread 
    , eventInfoImplOtherCap = Just otherCap
    }
  ThreadLabel {..} -> defImpl {
      eventInfoImplThread = Just thread 
    , eventInfoImplThreadlabel = Just threadlabel
    }
  CreateSparkThread {..} -> defImpl {
      eventInfoImplSparkThread = Just sparkThread
    }
  SparkCounters {..} -> defImpl {
      eventInfoImplSparksCreated = Just sparksCreated
    , eventInfoImplSparksDud = Just sparksDud
    , eventInfoImplSparksOverflowed = Just sparksOverflowed
    , eventInfoImplSparksConverted = Just sparksConverted
    , eventInfoImplSparksFizzled = Just sparksFizzled
    , eventInfoImplSparksGCd = Just sparksGCd
    , eventInfoImplSparksRemaining = Just sparksRemaining
    }
  SparkCreate -> defImpl
  SparkDud -> defImpl 
  SparkOverflow -> defImpl 
  SparkRun -> defImpl
  SparkSteal {..} -> defImpl {
      eventInfoImplVictimCap = Just victimCap
    }
  SparkFizzle -> defImpl
  SparkGC -> defImpl 
  TaskCreate {..} -> defImpl {
      eventInfoImplTaskId = Just taskId
    , eventInfoImplCap = Just cap
    , eventInfoImplTid = Just $ kernelThreadId tid
    }
  TaskMigrate {..} -> defImpl {
      eventInfoImplTaskId = Just taskId
    , eventInfoImplCap = Just cap
    , eventInfoImplNewCap = Just new_cap 
    }
  TaskDelete {..} -> defImpl {
      eventInfoImplTaskId = Just taskId
    }
  RequestSeqGC -> defImpl 
  RequestParGC -> defImpl 
  StartGC -> defImpl 
  GCWork -> defImpl 
  GCIdle -> defImpl 
  GCDone -> defImpl 
  EndGC -> defImpl 
  GlobalSyncGC -> defImpl 
  GCStatsGHC {..} -> defImpl {
      eventInfoImplHeapCapset = Just heapCapset
    , eventInfoImplGen = Just gen
    , eventInfoImplCopied = Just copied
    , eventInfoImplSlop = Just slop
    , eventInfoImplFrag = Just frag
    , eventInfoImplParNThreads = Just parNThreads
    , eventInfoImplParMaxCopied = Just parMaxCopied
    , eventInfoImplParTotCopied = Just parTotCopied
    }
  HeapAllocated {..} -> defImpl {
      eventInfoImplHeapCapset = Just heapCapset
    , eventInfoImplAllocBytes = Just allocBytes
    }
  HeapSize {..} -> defImpl {
      eventInfoImplHeapCapset = Just heapCapset
    , eventInfoImplSizeBytes = Just sizeBytes
    }
  HeapLive {..} -> defImpl {
      eventInfoImplHeapCapset = Just heapCapset
    , eventInfoImplLiveBytes = Just liveBytes
    }
  HeapInfoGHC {..} -> defImpl {
      eventInfoImplHeapCapset = Just heapCapset
    , eventInfoImplGens = Just gens
    , eventInfoImplMaxHeapSize = Just maxHeapSize
    , eventInfoImplAllocAreaSize = Just allocAreaSize
    , eventInfoImplMblockSize = Just mblockSize
    , eventInfoImplBlockSize = Just blockSize
    }
  CapCreate {..} -> defImpl {
      eventInfoImplCap = Just cap
    }
  CapDelete {..} -> defImpl {
      eventInfoImplCap = Just cap
    }
  CapDisable {..} -> defImpl {
      eventInfoImplCap = Just cap
    }
  CapEnable {..} -> defImpl {
      eventInfoImplCap = Just cap
    }
  CapsetCreate {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplCapsetType = Just $ toCapsetTypeImpl capsetType
    }
  CapsetDelete {..} -> defImpl {
      eventInfoImplCapset = Just capset
    }
  CapsetAssignCap {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplCap = Just cap
    }
  CapsetRemoveCap {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplCap = Just cap 
    }
  RtsIdentifier {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplRtsident = Just rtsident
    }
  ProgramArgs {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplArgs = Just args
    }
  ProgramEnv {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplEnv = Just env
    }
  OsProcessPid {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplPid = Just pid
    }
  OsProcessParentPid {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplPpid = Just ppid
    }
  WallClockTime {..} -> defImpl {
      eventInfoImplCapset = Just capset
    , eventInfoImplSec = Just sec
    , eventInfoImplNsec = Just nsec
    }
  Message {..} -> defImpl {
      eventInfoImplMsg = Just msg
    }
  UserMessage {..} -> defImpl {
      eventInfoImplMsg = Just msg
    }
  UserMarker {..} -> defImpl {
      eventInfoImplMarkername = Just markername
    }
  Version {..} -> defImpl {
      eventInfoImplVersion = Just version
    }
  ProgramInvocation {..} -> defImpl {
      eventInfoImplCommandline = Just commandline
    }
  CreateMachine {..} -> defImpl {
      eventInfoImplMachine = Just machine
    , eventInfoImplRealtime = Just realtime
    }
  KillMachine {..} -> defImpl {
      eventInfoImplMachine = Just machine 
    }
  CreateProcess {..} -> defImpl {
      eventInfoImplProcess = Just process
    }
  KillProcess {..} -> defImpl {
      eventInfoImplProcess = Just process
    }
  AssignThreadToProcess {..} -> defImpl {
      eventInfoImplThread = Just thread
    , eventInfoImplProcess = Just process
    }
  EdenStartReceive -> defImpl
  EdenEndReceive -> defImpl
  SendMessage {..} -> defImpl {
      eventInfoImplMesTag = Just $ toMessageTagImpl mesTag 
    , eventInfoImplSenderProcess = Just senderProcess
    , eventInfoImplSenderThread = Just senderThread 
    , eventInfoImplReceiverMachine = Just receiverMachine 
    , eventInfoImplReceiverProcess = Just receiverProcess 
    , eventInfoImplReceiverInport = Just receiverInport
    }
  ReceiveMessage {..} -> defImpl {
      eventInfoImplMesTag = Just $ toMessageTagImpl mesTag
    , eventInfoImplReceiverProcess = Just receiverProcess
    , eventInfoImplReceiverInport = Just receiverInport
    , eventInfoImplSenderMachine = Just senderMachine
    , eventInfoImplSenderProcess = Just senderProcess
    , eventInfoImplSenderThread = Just senderThread
    , eventInfoImplMessageSize = Just messageSize
    }
  SendReceiveLocalMessage {..} -> defImpl {
      eventInfoImplMesTag = Just $ toMessageTagImpl mesTag
    , eventInfoImplSenderProcess = Just senderProcess
    , eventInfoImplSenderThread = Just senderThread
    , eventInfoImplReceiverProcess = Just receiverProcess
    , eventInfoImplReceiverInport = Just receiverInport
    }
  InternString {..} -> defImpl {
      eventInfoImplStr = Just str 
    , eventInfoImplSId = Just sId
    }
  MerStartParConjunction {..} -> defImpl {
      eventInfoImplDynId = Just dyn_id
    , eventInfoImplStaticId = Just static_id
    }
  MerEndParConjunction {..} -> defImpl {
      eventInfoImplDynId = Just dyn_id
    }
  MerEndParConjunct {..} -> defImpl {
      eventInfoImplDynId = Just dyn_id
    }
  MerCreateSpark {..} -> defImpl {
      eventInfoImplDynId = Just dyn_id
    , eventInfoImplSparkId = Just spark_id
    }
  MerFutureCreate {..} -> defImpl {
      eventInfoImplFutureId = Just future_id
    , eventInfoImplNameId = Just name_id
    }
  MerFutureWaitNosuspend {..} -> defImpl {
      eventInfoImplFutureId = Just future_id
    }
  MerFutureWaitSuspended {..} -> defImpl {
      eventInfoImplFutureId = Just future_id
    }
  MerFutureSignal {..} -> defImpl {
      eventInfoImplFutureId = Just future_id
    }
  MerLookingForGlobalThread -> defImpl 
  MerWorkStealing  -> defImpl 
  MerLookingForLocalSpark -> defImpl 
  MerReleaseThread {..} -> defImpl {
      eventInfoImplThreadId = Just thread_id
    }
  MerCapSleeping -> defImpl
  MerCallingMain -> defImpl 
  PerfName {..} -> defImpl {
      eventInfoImplPerfNum = Just perfNum
    , eventInfoImplName = Just name
    }
  PerfCounter {..} -> defImpl {
      eventInfoImplPerfNum = Just perfNum
    , eventInfoImplTid = Just $ kernelThreadId tid
    , eventInfoImplPeriod = Just period
    }
  PerfTracepoint {..} -> defImpl {
      eventInfoImplPerfNum = Just perfNum
    , eventInfoImplTid = Just $ kernelThreadId tid
    }
  where 
  defImpl = emptyEventInfoImpl $ eventTypeNum e

-- | Helper to reconstruct event info from RDBMS representation
fromEventInfoImpl :: EventInfoImpl -> Maybe EventInfo
fromEventInfoImpl EventInfoImpl{..} = case eventInfoImplType of 
  EVENT_CREATE_THREAD -> CreateThread 
    <$> eventInfoImplThread
  EVENT_RUN_THREAD -> RunThread
    <$> eventInfoImplThread
  EVENT_STOP_THREAD -> StopThread
    <$> eventInfoImplThread
    <*> (fromThreadStopStatusImpl =<< eventInfoImplStatus)
  EVENT_THREAD_RUNNABLE -> ThreadRunnable
    <$> eventInfoImplThread
  EVENT_MIGRATE_THREAD -> MigrateThread
    <$> eventInfoImplThread
    <*> eventInfoImplNewCap
  EVENT_SHUTDOWN -> Just Shutdown
  EVENT_THREAD_WAKEUP -> WakeupThread
    <$> eventInfoImplThread
    <*> eventInfoImplOtherCap
  EVENT_THREAD_LABEL -> ThreadLabel
    <$> eventInfoImplThread
    <*> eventInfoImplThreadlabel
  EVENT_GC_START -> Just StartGC
  EVENT_GC_END -> Just EndGC
  EVENT_GC_GLOBAL_SYNC -> Just GlobalSyncGC
  EVENT_REQUEST_SEQ_GC -> Just RequestSeqGC
  EVENT_REQUEST_PAR_GC -> Just RequestParGC
  EVENT_CREATE_SPARK_THREAD -> CreateSparkThread
    <$> eventInfoImplSparkThread
  EVENT_SPARK_COUNTERS -> SparkCounters
    <$> eventInfoImplSparksCreated
    <*> eventInfoImplSparksDud
    <*> eventInfoImplSparksOverflowed
    <*> eventInfoImplSparksConverted
    <*> eventInfoImplSparksFizzled
    <*> eventInfoImplSparksGCd
    <*> eventInfoImplSparksRemaining
  EVENT_SPARK_CREATE -> Just SparkCreate
  EVENT_SPARK_DUD -> Just SparkDud
  EVENT_SPARK_OVERFLOW -> Just SparkOverflow
  EVENT_SPARK_RUN -> Just SparkRun
  EVENT_SPARK_STEAL -> SparkSteal
    <$> eventInfoImplVictimCap
  EVENT_SPARK_FIZZLE -> Just SparkFizzle
  EVENT_SPARK_GC -> Just SparkGC
  EVENT_TASK_CREATE -> TaskCreate
    <$> eventInfoImplTaskId
    <*> eventInfoImplCap 
    <*> (fmap KernelThreadId eventInfoImplTid)
  EVENT_TASK_MIGRATE -> TaskMigrate
    <$> eventInfoImplTaskId
    <*> eventInfoImplCap
    <*> eventInfoImplNewCap
  EVENT_TASK_DELETE -> TaskDelete
    <$> eventInfoImplTaskId
  EVENT_LOG_MSG -> Message 
    <$> eventInfoImplMsg 
  EVENT_STARTUP -> Startup
    <$> eventInfoImplNCaps
  EVENT_BLOCK_MARKER -> EventBlock
    <$> eventInfoImplEndTime
    <*> eventInfoImplCap 
    <*> (fmap fromIntegral eventInfoImplBlockSize)
  EVENT_USER_MSG -> UserMessage
    <$> eventInfoImplMsg 
  EVENT_USER_MARKER -> UserMarker
    <$> eventInfoImplMarkername
  EVENT_GC_IDLE -> Just GCIdle
  EVENT_GC_WORK -> Just GCWork
  EVENT_GC_DONE -> Just GCDone
  EVENT_GC_STATS_GHC -> GCStatsGHC
    <$> eventInfoImplHeapCapset
    <*> eventInfoImplGen 
    <*> eventInfoImplCopied
    <*> eventInfoImplSlop
    <*> eventInfoImplFrag
    <*> eventInfoImplParNThreads
    <*> eventInfoImplParMaxCopied
    <*> eventInfoImplParTotCopied
  EVENT_HEAP_ALLOCATED -> HeapAllocated
    <$> eventInfoImplHeapCapset
    <*> eventInfoImplAllocBytes
  EVENT_HEAP_SIZE -> HeapSize
    <$> eventInfoImplHeapCapset
    <*> eventInfoImplSizeBytes
  EVENT_HEAP_LIVE -> HeapLive
    <$> eventInfoImplHeapCapset
    <*> eventInfoImplLiveBytes
  EVENT_HEAP_INFO_GHC -> HeapInfoGHC
    <$> eventInfoImplHeapCapset
    <*> eventInfoImplGens 
    <*> eventInfoImplMaxHeapSize
    <*> eventInfoImplAllocAreaSize
    <*> eventInfoImplMblockSize
    <*> eventInfoImplBlockSize
  EVENT_CAP_CREATE -> CapCreate
    <$> eventInfoImplCap 
  EVENT_CAP_DELETE -> CapDelete
    <$> eventInfoImplCap 
  EVENT_CAP_DISABLE -> CapDisable
    <$> eventInfoImplCap 
  EVENT_CAP_ENABLE -> CapEnable
    <$> eventInfoImplCap 
  EVENT_CAPSET_CREATE -> CapsetCreate
    <$> eventInfoImplCapset
    <*> (fromCapsetTypeImpl =<< eventInfoImplCapsetType)
  EVENT_CAPSET_DELETE -> CapsetDelete
    <$> eventInfoImplCapset
  EVENT_CAPSET_ASSIGN_CAP -> CapsetAssignCap
    <$> eventInfoImplCapset
    <*> eventInfoImplCap
  EVENT_CAPSET_REMOVE_CAP -> CapsetRemoveCap
    <$> eventInfoImplCapset 
    <*> eventInfoImplCap
  EVENT_RTS_IDENTIFIER -> RtsIdentifier
    <$> eventInfoImplCapset 
    <*> eventInfoImplRtsident
  EVENT_PROGRAM_ARGS -> ProgramArgs
    <$> eventInfoImplCapset 
    <*> eventInfoImplArgs
  EVENT_PROGRAM_ENV -> ProgramEnv
    <$> eventInfoImplCapset 
    <*> eventInfoImplEnv
  EVENT_OSPROCESS_PID -> OsProcessPid
    <$> eventInfoImplCapset 
    <*> eventInfoImplPid
  EVENT_OSPROCESS_PPID -> OsProcessParentPid
    <$> eventInfoImplCapset 
    <*> eventInfoImplPpid 
  EVENT_WALL_CLOCK_TIME -> WallClockTime
    <$> eventInfoImplCapset
    <*> eventInfoImplSec 
    <*> eventInfoImplNsec
  EVENT_INTERN_STRING -> InternString 
    <$> eventInfoImplStr 
    <*> eventInfoImplSId
  EVENT_VERSION -> Version
    <$> eventInfoImplVersion
  EVENT_PROGRAM_INVOCATION -> ProgramInvocation
    <$> eventInfoImplCommandline
  EVENT_EDEN_START_RECEIVE -> Just EdenStartReceive
  EVENT_EDEN_END_RECEIVE -> Just EdenEndReceive
  EVENT_CREATE_PROCESS -> CreateProcess
    <$> eventInfoImplProcess
  EVENT_KILL_PROCESS -> KillProcess
    <$> eventInfoImplProcess
  EVENT_ASSIGN_THREAD_TO_PROCESS -> AssignThreadToProcess
    <$> eventInfoImplThread
    <*> eventInfoImplProcess
  EVENT_CREATE_MACHINE -> CreateMachine
    <$> eventInfoImplMachine
    <*> eventInfoImplRealtime
  EVENT_KILL_MACHINE -> KillMachine
    <$> eventInfoImplMachine
  EVENT_SEND_MESSAGE -> SendMessage
    <$> (fromMessageTagImpl =<< eventInfoImplMesTag)
    <*> eventInfoImplSenderProcess
    <*> eventInfoImplSenderThread
    <*> eventInfoImplReceiverMachine
    <*> eventInfoImplReceiverProcess
    <*> eventInfoImplReceiverInport
  EVENT_RECEIVE_MESSAGE -> ReceiveMessage
    <$> (fromMessageTagImpl =<< eventInfoImplMesTag)
    <*> eventInfoImplReceiverProcess
    <*> eventInfoImplReceiverInport
    <*> eventInfoImplSenderMachine
    <*> eventInfoImplSenderProcess
    <*> eventInfoImplSenderThread
    <*> eventInfoImplMessageSize
  EVENT_SEND_RECEIVE_LOCAL_MESSAGE -> SendReceiveLocalMessage
    <$> (fromMessageTagImpl =<< eventInfoImplMesTag)
    <*> eventInfoImplSenderProcess
    <*> eventInfoImplSenderThread
    <*> eventInfoImplReceiverProcess
    <*> eventInfoImplReceiverInport
  EVENT_MER_START_PAR_CONJUNCTION -> MerStartParConjunction
    <$> eventInfoImplDynId
    <*> eventInfoImplStaticId
  EVENT_MER_STOP_PAR_CONJUNCTION -> MerEndParConjunction
    <$> eventInfoImplDynId
  EVENT_MER_STOP_PAR_CONJUNCT -> MerEndParConjunct
    <$> eventInfoImplDynId
  EVENT_MER_CREATE_SPARK -> MerCreateSpark
    <$> eventInfoImplDynId
    <*> eventInfoImplSparkId
  EVENT_MER_FUT_CREATE -> MerFutureCreate
    <$> eventInfoImplFutureId
    <*> eventInfoImplNameId
  EVENT_MER_FUT_WAIT_NOSUSPEND -> MerFutureWaitNosuspend
    <$> eventInfoImplFutureId
  EVENT_MER_FUT_WAIT_SUSPENDED -> MerFutureWaitSuspended
    <$> eventInfoImplFutureId
  EVENT_MER_FUT_SIGNAL -> MerFutureSignal
    <$> eventInfoImplFutureId
  EVENT_MER_LOOKING_FOR_GLOBAL_CONTEXT -> Just MerLookingForGlobalThread
  EVENT_MER_WORK_STEALING -> Just MerWorkStealing
  EVENT_MER_LOOKING_FOR_LOCAL_SPARK -> Just MerLookingForLocalSpark
  EVENT_MER_RELEASE_CONTEXT -> MerReleaseThread
    <$> eventInfoImplThreadId
  EVENT_MER_ENGINE_SLEEPING -> Just MerCapSleeping
  EVENT_MER_CALLING_MAIN -> Just MerCallingMain
  nEVENT_PERF_NAME -> PerfName
    <$> eventInfoImplPerfNum
    <*> eventInfoImplName
  nEVENT_PERF_COUNTER -> PerfCounter
    <$> eventInfoImplPerfNum
    <*> (fmap KernelThreadId eventInfoImplTid) 
    <*> eventInfoImplPeriod
  nEVENT_PERF_TRACEPOINT -> PerfTracepoint
    <$> eventInfoImplPerfNum
    <*> (fmap KernelThreadId eventInfoImplTid)

  _ -> UnknownEvent <$> eventInfoImplRef

eventTypeNum :: EventInfo -> EventTypeNum
eventTypeNum e = case e of
    CreateThread {} -> EVENT_CREATE_THREAD
    RunThread {} -> EVENT_RUN_THREAD
    StopThread {} -> EVENT_STOP_THREAD
    ThreadRunnable {} -> EVENT_THREAD_RUNNABLE
    MigrateThread {} -> EVENT_MIGRATE_THREAD
    Shutdown {} -> EVENT_SHUTDOWN
    WakeupThread {} -> EVENT_THREAD_WAKEUP
    ThreadLabel {}  -> EVENT_THREAD_LABEL
    StartGC {} -> EVENT_GC_START
    EndGC {} -> EVENT_GC_END
    GlobalSyncGC {} -> EVENT_GC_GLOBAL_SYNC
    RequestSeqGC {} -> EVENT_REQUEST_SEQ_GC
    RequestParGC {} -> EVENT_REQUEST_PAR_GC
    CreateSparkThread {} -> EVENT_CREATE_SPARK_THREAD
    SparkCounters {} -> EVENT_SPARK_COUNTERS
    SparkCreate   {} -> EVENT_SPARK_CREATE
    SparkDud      {} -> EVENT_SPARK_DUD
    SparkOverflow {} -> EVENT_SPARK_OVERFLOW
    SparkRun      {} -> EVENT_SPARK_RUN
    SparkSteal    {} -> EVENT_SPARK_STEAL
    SparkFizzle   {} -> EVENT_SPARK_FIZZLE
    SparkGC       {} -> EVENT_SPARK_GC
    TaskCreate  {} -> EVENT_TASK_CREATE
    TaskMigrate {} -> EVENT_TASK_MIGRATE
    TaskDelete  {} -> EVENT_TASK_DELETE
    Message {} -> EVENT_LOG_MSG
    Startup {} -> EVENT_STARTUP
    EventBlock {} -> EVENT_BLOCK_MARKER
    UserMessage {} -> EVENT_USER_MSG
    UserMarker  {} -> EVENT_USER_MARKER
    GCIdle {} -> EVENT_GC_IDLE
    GCWork {} -> EVENT_GC_WORK
    GCDone {} -> EVENT_GC_DONE
    GCStatsGHC{} -> EVENT_GC_STATS_GHC
    HeapAllocated{} -> EVENT_HEAP_ALLOCATED
    HeapSize{} -> EVENT_HEAP_SIZE
    HeapLive{} -> EVENT_HEAP_LIVE
    HeapInfoGHC{} -> EVENT_HEAP_INFO_GHC
    CapCreate{} -> EVENT_CAP_CREATE
    CapDelete{} -> EVENT_CAP_DELETE
    CapDisable{} -> EVENT_CAP_DISABLE
    CapEnable{} -> EVENT_CAP_ENABLE
    CapsetCreate {} -> EVENT_CAPSET_CREATE
    CapsetDelete {} -> EVENT_CAPSET_DELETE
    CapsetAssignCap {} -> EVENT_CAPSET_ASSIGN_CAP
    CapsetRemoveCap {} -> EVENT_CAPSET_REMOVE_CAP
    RtsIdentifier {} -> EVENT_RTS_IDENTIFIER
    ProgramArgs {} -> EVENT_PROGRAM_ARGS
    ProgramEnv {} -> EVENT_PROGRAM_ENV
    OsProcessPid {} -> EVENT_OSPROCESS_PID
    OsProcessParentPid{} -> EVENT_OSPROCESS_PPID
    WallClockTime{} -> EVENT_WALL_CLOCK_TIME
    UnknownEvent ref -> ref
    InternString {} -> EVENT_INTERN_STRING
    Version {} -> EVENT_VERSION
    ProgramInvocation {} -> EVENT_PROGRAM_INVOCATION
    EdenStartReceive {} -> EVENT_EDEN_START_RECEIVE
    EdenEndReceive {} -> EVENT_EDEN_END_RECEIVE
    CreateProcess {} -> EVENT_CREATE_PROCESS
    KillProcess {} -> EVENT_KILL_PROCESS
    AssignThreadToProcess {} -> EVENT_ASSIGN_THREAD_TO_PROCESS
    CreateMachine {} -> EVENT_CREATE_MACHINE
    KillMachine {} -> EVENT_KILL_MACHINE
    SendMessage {} -> EVENT_SEND_MESSAGE
    ReceiveMessage {} -> EVENT_RECEIVE_MESSAGE
    SendReceiveLocalMessage {} -> EVENT_SEND_RECEIVE_LOCAL_MESSAGE
    MerStartParConjunction {} -> EVENT_MER_START_PAR_CONJUNCTION
    MerEndParConjunction _ -> EVENT_MER_STOP_PAR_CONJUNCTION
    MerEndParConjunct _ -> EVENT_MER_STOP_PAR_CONJUNCT
    MerCreateSpark {} -> EVENT_MER_CREATE_SPARK
    MerFutureCreate {} -> EVENT_MER_FUT_CREATE
    MerFutureWaitNosuspend _ -> EVENT_MER_FUT_WAIT_NOSUSPEND
    MerFutureWaitSuspended _ -> EVENT_MER_FUT_WAIT_SUSPENDED
    MerFutureSignal _ -> EVENT_MER_FUT_SIGNAL
    MerLookingForGlobalThread -> EVENT_MER_LOOKING_FOR_GLOBAL_CONTEXT
    MerWorkStealing -> EVENT_MER_WORK_STEALING
    MerLookingForLocalSpark -> EVENT_MER_LOOKING_FOR_LOCAL_SPARK
    MerReleaseThread _ -> EVENT_MER_RELEASE_CONTEXT
    MerCapSleeping -> EVENT_MER_ENGINE_SLEEPING
    MerCallingMain -> EVENT_MER_CALLING_MAIN
    PerfName       {} -> nEVENT_PERF_NAME
    PerfCounter    {} -> nEVENT_PERF_COUNTER
    PerfTracepoint {} -> nEVENT_PERF_TRACEPOINT