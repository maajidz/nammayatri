module Tools.Metrics.AllocatorMetrics.Types
  ( HasAllocatorMetrics,
    AllocatorMetricsContainer (..),
    module CoreMetrics,
    registerAllocatorMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Utils.Common
import Prometheus as P

type HasAllocatorMetrics m r = (HasFlowEnv m r '["btmMetrics" ::: AllocatorMetricsContainer])

type TaskCounterMetric = P.Vector P.Label1 P.Counter

type TaskDurationMetric = P.Vector P.Label1 P.Histogram

type FailedTaskCounterMetric = P.Vector P.Label1 P.Counter

data AllocatorMetricsContainer = AllocatorMetricsContainer
  { taskCounter :: TaskCounterMetric,
    taskDuration :: TaskDurationMetric,
    failedTaskCounter :: FailedTaskCounterMetric
  }

registerAllocatorMetricsContainer :: IO AllocatorMetricsContainer
registerAllocatorMetricsContainer = do
  taskCounter <- registerTaskCounter
  taskDuration <- registerTaskDurationMetric
  failedTaskCounter <- registerFailedTaskCounter
  return $ AllocatorMetricsContainer {..}

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.vector "agency_name" . P.counter $ P.Info "BTM_task_count" ""

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.vector "agency_name" . P.counter $ P.Info "BTM_failed_task_count" ""

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.vector "agency_name" . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20
