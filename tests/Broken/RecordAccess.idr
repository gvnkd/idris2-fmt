module Reference

record Resp where
  task : Maybe String
  issue : Maybe String

foo : Resp -> Maybe String
foo r =r.task
