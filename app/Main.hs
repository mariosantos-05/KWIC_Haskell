import KWIC (splitWords, removeStopWords, generateCircularShifts, sortShifts, formatOutput)
import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
  -- Obter os argumentos passados para o programa
  args <- getArgs
  
  -- Verificar se o usuário forneceu um caminho para o arquivo
  when (null args) $ do
    putStrLn "Por favor, forneça o caminho para o arquivo de texto como argumento."
    return ()
  
  -- Ler o arquivo de texto fornecido
  let filePath = head args
  content <- readFile filePath

  -- Dividir o conteúdo do arquivo em linhas
  let linesInContent = lines content
  
  -- Processar o título para gerar shifts circulares
  let shifts = generateCircularShifts linesInContent

  -- Ordenar os shifts
  let sortedShifts = sortShifts shifts
  
  -- Formatar a saída
  let formattedOutput = formatOutput sortedShifts
  
  -- Exibir o resultado formatado
  putStrLn formattedOutput
