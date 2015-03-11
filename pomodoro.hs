import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_, void)
import System.Environment
import System.Process

newtype Minutes = Minutes Int

main :: IO ()
main = do
    task <- fmap extractTask getArgs
    notify task 25
    pomodoro task
    void . createProcess . shell $ "notify-send 'Finished!'"

extractTask :: [String] -> String
extractTask [] = "Working on unknown task"
extractTask t = filter (/='\'') . unwords $ t

sleep :: Minutes -> IO ()
sleep (Minutes m) = replicateM_ m (threadDelay minuteMicroseconds)
    where
        minuteMicroseconds = 60000000

pomodoro :: String -> IO ()
pomodoro task = do
    mapM_ (\remaining -> sleep (Minutes 5) >> notify task remaining) [20, 15, 10, 5]
    mapM_ (\remaining -> sleep (Minutes 1) >> notify task remaining) [4, 3, 2, 1, 0]

notify :: String -> Integer -> IO ()
notify task remaining = void . createProcess . shell $
    "notify-send '" ++ task ++ " (" ++ show remaining ++ " minutes remain)'"
