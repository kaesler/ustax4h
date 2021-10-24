module Debug (myFail) where

myFail :: () -> a
myFail = error "boom"
