module Debug (myFail) where
  
import GHC.Stack (HasCallStack)

myFail :: HasCallStack => ()  -> a
myFail = error "boom"
