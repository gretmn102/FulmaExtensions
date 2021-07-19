module Fulma.Extensions.Types

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't
