namespace Orchid

open System

type private Agent<'T> = MailboxProcessor<'T>

type private LogMessage =
    | Warn    of Type * string
    | WarnF   of Type * string * obj[]
    | Debug   of Type * string
    | DebugF  of Type * string * obj[]
    | Error   of Type * string
    | ErrorF  of Type * string * obj[]
    | Info    of Type * string
    | InfoF   of Type * string * obj[]
    | Cancel

[<AutoOpen>]
type Logger() =

    /// Gets a logger for the given type
    static let getLogger (t: Type) = log4net.LogManager.GetLogger(t)   

    /// Internal lgging agent
    static let logger = Agent.Start(fun inbox -> 
        let rec loop() =
            async { 
                let! msg = inbox.Receive()
                let cancel = ref false
                match msg with
                | LogMessage.Debug(t, msg)           -> getLogger(t).Debug(msg)
                | LogMessage.Error(t, msg)           -> getLogger(t).Error(msg)
                | LogMessage.Info(t, msg)            -> getLogger(t).Info(msg)
                | LogMessage.Warn(t, msg)            -> getLogger(t).Warn(msg)
                | LogMessage.DebugF(t, format, args) -> getLogger(t).DebugFormat(format, args)
                | LogMessage.ErrorF(t, format, args) -> getLogger(t).ErrorFormat(format, args)
                | LogMessage.InfoF(t, format, args)  -> getLogger(t).InfoFormat(format, args)
                | LogMessage.WarnF(t, format, args)  -> getLogger(t).WarnFormat(format, args)
                | Cancel                             -> cancel := true
                if not !cancel then return! loop() }
        loop())

    /// Logs a warning string produced by invoking f when the configured logger for type t is set 
    /// with warnings enabled
    static member LazyWarn (t: Type, f: unit -> string) =
        if getLogger(t).IsWarnEnabled then logger.Post(Warn(t, f()))

    /// Logs a debug string produced by invoking f when the configured logger for type t is set 
    /// with debug enabled
    static member LazyDebug (t: Type, f: unit -> string) =
        if getLogger(t).IsDebugEnabled then logger.Post(Debug(t, f()))

    /// Logs an error string produced by invoking f when the configured logger for type t is set 
    /// with errors enabled
    static member LazyError (t: Type, f: unit -> string) =
        if getLogger(t).IsErrorEnabled then logger.Post(Error(t, f()))

    /// Logs an info string produced by invoking f when the configured logger for type t is set 
    /// with info enabled
    static member LazyInfo (t: Type, f: unit -> string) =
        if getLogger(t).IsInfoEnabled then logger.Post(Info(t, f()))

    /// Logs a formatted warning message
    static member WarnF (t: Type, format:string, [<ParamArray>] args: obj[]) = logger.Post(WarnF(t, format, args))

    /// Logs a warning message
    static member Warn (t: Type, msg:string) = logger.Post(Warn(t, msg))
    
    /// Logs a formatted debug message
    static member DebugF (t: Type, format:string, [<ParamArray>] args: obj[]) = logger.Post(DebugF(t, format, args))
    
    /// Logs a debug message
    static member Debug (t: Type, msg:string) = logger.Post(Debug(t, msg))
    
    /// Logs a formatted info message
    static member InfoF (t: Type, format:string, [<ParamArray>] args: obj[]) = logger.Post(InfoF(t, format, args))

    /// Logs an info message
    static member Info (t: Type, msg:string) = logger.Post(Info(t, msg))

    /// Logs a formatted error message
    static member ErrorF (t: Type, format:string, [<ParamArray>] args: obj[]) = logger.Post(ErrorF(t, format, args))
    
    /// Logs an error message
    static member Error (t: Type, msg:string) = logger.Post(Error(t, msg))

    /// Logs an exception
    static member Error (t: Type, ex:Exception) = logger.Post(Error(t, ex.ToString()))

    /// Cancels the logger
    static member Cancel() = logger.Post(Cancel)

module Logging =
    
    /// Logs and re-raises any caught exception
    let logReraise t f=
        try
            f()
        with
            exn as e -> 
                Logger.ErrorF(t, "Caught exception: ", e.ToString())
                reraise()

    /// Handles any exception raised by invoking f, logging the error out to the configured logger.
    /// This function should be called very sparingly, as it will catch any exception type
    let logExn t f=
        try
            f()
        with
            exn as e -> Logger.ErrorF(t, "Caught exception: ", e.ToString())