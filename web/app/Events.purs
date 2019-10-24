module App.Events 
	( Event(..)
	) where

data Event = Increment | Decrement | UserClick String | Reset