--Test type
type NumChoices = Int
type CorrectChoice = Int
type CorrectByModel = [CorrectChoice]
data Question = Question NumChoices CorrectByModel deriving Show
type QuestionList = [Question]
data Test = Test QuestionList deriving Show

--RespuestasTest data type
type ChoiceMade = Int --We are going to consider 0 as a blank answer to a question from the student
type DNI = Int
type TestModel = Int
type Responses = [ChoiceMade]
data RespuestaTest = RespuestaTest DNI TestModel Responses deriving Show

--Correccion data type
type CorrectAnswers = Int
type Grade = Float
data Correccion = Correccion DNI CorrectAnswers Grade deriving Show

--Frecuencias data type
type FrecAbsAciert = Int
type FrecAbsFallos = Int
type FrecAbsBlanco = Int
type FrecRelAciert = Float
type FrecRelFallos = Float
type FrecRelBlanco = Float
data FrecuenciaPorPregunta = Frecuencias FrecAbsAciert FrecAbsFallos FrecAbsBlanco FrecRelAciert FrecRelFallos FrecRelBlanco deriving Show
type TodasFrecuencias = [FrecuenciaPorPregunta]


--Estadisticas data type
type MeanScore = Float
type MeanNumber = Float
type Suspensos = Int
type Aprobados = Int
type Notables = Int
type Sobresalientes = Int
type MejoresPreguntas = [Int] --En este caso lo he hecho en plural porque se puede dar el caso de que haya varias preguntas siendo la mejor o la peor
type PeoresPreguntas = [Int]
type PreguntasMasBlanco = [Int]
type PreguntasMenosBlanco = [Int]
data Estadisticas = Estadisticas MeanScore MeanNumber Suspensos Aprobados Notables Sobresalientes [Frequencies] MejoresPreguntas PeoresPreguntas PreguntasMasBlanco PreguntasMenosBlanco deriving Show

--Frecuencias de una pregunta
type FAbsRight = Int
type FRelRight = Float
type FAbsWrong = Int
type FRelWrong = Float
type FAbsBlank = Int
type FRelBlank = Float
type Frequencies = (FAbsRight,FRelRight,FAbsWrong,FRelWrong,FAbsBlank,FRelBlank)

--otros tipes de datos
type ModelAndChoice = (TestModel,ChoiceMade)

--Getters
getCorrectByModel::Question->CorrectByModel
getCorrectByModel (Question _ choices) = choices

getQuestions::Test->QuestionList
getQuestions (Test questions) = questions

getDNI::RespuestaTest->DNI
getDNI (RespuestaTest dni _ _) = dni

getResponses::RespuestaTest->Responses
getResponses (RespuestaTest _ _ responses) = responses

getModel::RespuestaTest->TestModel
getModel (RespuestaTest _ testModel _) = testModel

getNumChoices::Question->NumChoices
getNumChoices (Question numChoices _) = numChoices

getGrade::Correccion->Float
getGrade (Correccion _ _ grade) = grade

getModelModelAndChoice::ModelAndChoice->TestModel
getModelModelAndChoice tuple = fst tuple

getChoice::ModelAndChoice->ChoiceMade
getChoice tuple = snd tuple

--Other usefull functions

getNumQuestion::Test->Int
getNumQuestion (Test questions) = (length questions)

getNumQuestionFloat::Test->Float
getNumQuestionFloat (Test questions) = fromIntegral (length questions) :: Float



------------------------------------------------------------
--First part: corrige and all the auxiliary functions needed
------------------------------------------------------------
corrige::Test->RespuestaTest->Correccion
corrige test responseTest = Correccion (getDNI responseTest) m n
                            where (m,n) = tupleWrapper (map getNumChoices (getQuestions test)) (map (correctForAModel (getModel responseTest)) (getQuestions test)) (getResponses responseTest) (getNumQuestion test)

--Esta funcion simplemente es para juntar en una tupla el numero de respuestas correctas y la nota de corregir un examen.
tupleWrapper::[NumChoices]->[CorrectChoice]->Responses->Int->(Int,Float)
tupleWrapper numchoices correctchoices responses n = (countEqualsQuestions correctchoices responses ,corrigeAux numchoices correctchoices responses n)

--Cuenta el numero de preguntas acertadas 
countEqualsQuestions::[CorrectChoice]->Responses->Int
countEqualsQuestions [] _ = 0
countEqualsQuestions _ [] = 0
countEqualsQuestions (x:xs) (y:ys)
      | x == y = 1 + countEqualsQuestions xs ys
      | otherwise = countEqualsQuestions xs ys

--Calcula la nota sacada por un alumno en el examen
corrigeAux::[NumChoices]->[CorrectChoice]->Responses->Int->Float
corrigeAux [] [] _ _ = 0.0
corrigeAux _ _ [] _ = 0.0
corrigeAux (x:xs) (y:ys) (z:zs) n
      | z == 0 = corrigeAux xs ys zs n
      | y == z = corrigeAux xs ys zs n + (10.0 / (fromIntegral n))
      | otherwise = corrigeAux xs ys zs n - (1.0 / (fromIntegral x))

--Devuelve para un modelo cual es la respuesta correcta a una pregunta.
correctForAModel::Int->Question->CorrectChoice
correctForAModel n question  = (getCorrectByModel question)!!(n-1)


------------------------------------------------------------------------------------------
--Second part: Corregir un conjunto de examenes y devolver las estadistcas de todos ellos.
------------------------------------------------------------------------------------------
estadisticas::Test->[RespuestaTest]->Estadisticas
estadisticas test responsesTest = Estadisticas (mediaFloat (map getGrade resultList)) (mediaInt (map numRespondidas responsesTest)) (numSuspensos resultList) (numAprobados resultList) (numNotables resultList) (numSobresalientes resultList) frecList mejores peores masBlanco menosBlanco 
                                  where
                                       resultList = map (corrige test) responsesTest
                                       frecList = listStatistics test responsesTest
                                       (mejores,peores,masBlanco,menosBlanco) = ((mejoresPreguntas frecList), (peoresPregunta frecList), (masEnBlanco frecList), (menosEnBlanco frecList))


--Tres funciones auxiliares que son una suma de una lista y las dos medias que solo se diferencian en el tipo de entrada y de la salida
suma::(Fractional a, Integral a1)=>[a1]->a
suma [] = 0.0
suma (x:xs) = (fromIntegral x) + suma xs

mediaInt::(Fractional a, Integral a1)=>[a1]->a
mediaInt x = suma x / (fromIntegral (length x))

mediaFloat::[Float]->Float
mediaFloat x = sum x / (fromIntegral (length x))

--Estas cuatro funciones son filters de todas las notas sacadas y de esa manera puedo clasificarlos facil en las 4 categorias segun la nota.
numSuspensos::[Correccion]->Int
numSuspensos x = length(filter (<5) (map getGrade x))

numAprobados::[Correccion]->Int
numAprobados x = length( filter (<7) (filter (>=5) (map getGrade x)))

numNotables::[Correccion]->Int
numNotables x = length( filter (<9) (filter (>=7) (map getGrade x)))

numSobresalientes::[Correccion]->Int
numSobresalientes x = length(filter (>=9) (map getGrade x))

--Dado las respuestas a un examen de un alumno devuelve cuantas de ellas ha respondido es decir todas las que no estan en blanco.
numRespondidas::RespuestaTest->Int
numRespondidas respuestaTest = numRespondidasAux (getResponses respuestaTest)

numRespondidasAux::Responses->Int
numRespondidasAux [] = 0
numRespondidasAux (x:xs)
                 | x == 0 = numRespondidasAux xs
                 | otherwise = (1+numRespondidasAux xs)


--listStatistics devuelve la lista de las estadisticas para cada posicion para una pregunta diferente.
listStatistics::Test->[RespuestaTest]->[Frequencies]
listStatistics test listResponses = [statisticsForAQuestion (getCorrectByModel ((getQuestions test) !! i)) (modelAndChoiceByNum !! i) | i <-[0..(length (getQuestions test)-1)]]
                                    where
                                        listModelAndChoices = map responsesAndModelByNum listResponses
                                        modelAndChoiceByNum = [ (map (!!i) listModelAndChoices) | i<-[0..(length (getQuestions test)-1)]]

--Dadas las respuestas a un examen de un alumno hace una lista de tuplas con el modelo del examen del alumno junto a su respuesta al examen.
responsesAndModelByNum::RespuestaTest->[ModelAndChoice]
responsesAndModelByNum respuesta = zip (take (length (getResponses respuesta)) (repeat (getModel respuesta))) (getResponses respuesta)

--The list that this function receives is already a list of all the testModel and choices made for a concrete question and the correcte answers by model for that question
statisticsForAQuestion::CorrectByModel->[ModelAndChoice]->Frequencies
statisticsForAQuestion correctByModel modelsAndChoices = statisticsAux correctByModel modelsAndChoices (length modelsAndChoices) 0 0 0

statisticsAux::CorrectByModel->[ModelAndChoice]->Int->FAbsRight->FAbsWrong->FAbsBlank->Frequencies
statisticsAux _ [] length x y z = (x,((fromIntegral x)/(fromIntegral length)),y,((fromIntegral y)/(fromIntegral length)),z,((fromIntegral z)/(fromIntegral length)))
statisticsAux correct (t:ts) length x y z
                      |(getChoice t) == 0 = statisticsAux correct ts length x y (z+1)
                      |(getChoice t) == correct!!((getModelModelAndChoice t)-1) = statisticsAux correct ts length (x+1) y z
                      |otherwise = statisticsAux correct ts length x (y+1) z

--Funciontes para conseguir las mejores/peores y las que mas/menos veces se han dejado en blanco. Las he tomado como una lista porque puede haber varias preguntas con las mejores estadisticas
--He considerado la mejor pregunta aquella que mas veces se ha acertado y en caso de empate en aciertos la que mas veces se ha dejado en blanco(respectivamente menos veces se ha fallado)
mejoresPreguntas::[Frequencies]->[Int]
mejoresPreguntas (f:fs) = mejoresPregAux fs f 2 [1]

mejoresPregAux::[Frequencies]->Frequencies->Int->[Int]->[Int]
mejoresPregAux [] _ _ ys = ys
mejoresPregAux (x:xs) bestQuest n ys
            |(compareQuestionResult bestQuest x) == 0 = mejoresPregAux xs bestQuest (n+1) (n:ys)
            |(compareQuestionResult bestQuest x) == 1 = mejoresPregAux xs x (n+1) [n]
            |otherwise = mejoresPregAux xs bestQuest (n+1) ys

peoresPregunta::[Frequencies]->[Int]
peoresPregunta (f:fs) = peoresPregAux fs f 2 [1]

peoresPregAux::[Frequencies]->Frequencies->Int->[Int]->[Int]
peoresPregAux [] _ _ ys = ys
peoresPregAux (x:xs) worstQuest n ys
            |(compareQuestionResult worstQuest x) == 0 = peoresPregAux xs worstQuest (n+1) (n:ys)
            |(compareQuestionResult worstQuest x) == -1 = peoresPregAux xs x (n+1) [n]
            |otherwise = peoresPregAux xs worstQuest (n+1) ys


--Devuelve una tupla, el primer numero indica 0 si son iguales, -1 si el segundo es menor y 1 si el segundo es mayor. 
compareQuestionResult::Frequencies->Frequencies->Int
compareQuestionResult x y = if (fAbsRightNum x) > (fAbsRightNum y)
                            then -1
                            else if (fAbsRightNum x) < (fAbsRightNum y)
                                then 1
                                else if ((fAbsRightNum x) == (fAbsRightNum y)) && ((fAbsBlankNum x) > (fAbsBlankNum y))
                                    then -1
                                    else if ((fAbsRightNum x) == (fAbsRightNum y)) && ((fAbsBlankNum x) < (fAbsBlankNum y))
                                        then 1
                                        else 0

--Por ultimo las funciones para la pregunta que mas veces se ha dejado en blanco y la que menos
masEnBlanco::[Frequencies]->[Int]
masEnBlanco (f:fs)  = masEnBlancoAux fs f 2 [1]

masEnBlancoAux::[Frequencies]->Frequencies->Int->[Int]->[Int]
masEnBlancoAux [] _ _ ys = ys
masEnBlancoAux (x:xs) mostBlank n ys
            |(fAbsBlankNum x) > (fAbsBlankNum mostBlank) = masEnBlancoAux xs x (n+1) [n]
            |(fAbsBlankNum x) == (fAbsBlankNum mostBlank) = masEnBlancoAux xs mostBlank (n+1) (n:ys)
            |otherwise = masEnBlancoAux xs mostBlank (n+1) ys

menosEnBlanco::[Frequencies]->[Int]
menosEnBlanco (f:fs)  = menosEnBlancoAux fs f 2 [1]

menosEnBlancoAux::[Frequencies]->Frequencies->Int->[Int]->[Int]
menosEnBlancoAux [] _ _ ys = ys
menosEnBlancoAux (x:xs) lessBlank n ys
            |(fAbsBlankNum x) < (fAbsBlankNum lessBlank) = menosEnBlancoAux xs x (n+1) [n]
            |(fAbsBlankNum x) == (fAbsBlankNum lessBlank) = menosEnBlancoAux xs lessBlank (n+1) (n:ys)
            |otherwise = menosEnBlancoAux xs lessBlank (n+1) ys


--Dado unas Estadisticas printearlas de una manera facil de entender en la pantalla 
printEstadisticas::Estadisticas->IO ()
printEstadisticas x = do
                       putStrLn ("Nota media del examen: " ++ (show (getMedia x)))
                       putStrLn ("Numero medio de preguntas respondidas: " ++ (show (getMediaPreguntas x)))
                       putStrLn ("Numero suspensos: " ++ (show (getSuspensos x)))
                       putStrLn ("Numero aprobados: " ++ (show (getAprobados x)))
                       putStrLn ("Numero notables: " ++ (show (getNotables x)))
                       putStrLn ("Numero sobresalientes: " ++ (show (getSobresalientes x)))
                       printFrecRecursivo (zip (take (length (getListFrec x) ) (iterate (+1) 1)) (getListFrec x))
                       putStrLn ("Las mejores preguntas son: " ++ (show (getMejoresPreg x)))
                       putStrLn ("Las peores preguntas son: " ++ (show (getPeoresPreg x)))
                       putStrLn ("Las preguntas mas veces dejadas en blanco son: " ++ (show (getMasBlanco x)))
                       putStrLn ("Las preguntas menos veces dejadas en blanco son: " ++ (show (getMenosBlanco x)))

--Las dos funciones para printear los datos de la frecuencia de las preguntas que viene dado en una lista en la que cada posicion son las estadisticas de una pregunta.
printFrecRecursivo::[(Int,Frequencies)]->IO ()
printFrecRecursivo [] = putStrLn("")
printFrecRecursivo (x:xs) = do
                             printFrecuencias x
                             printFrecRecursivo xs

printFrecuencias::(Int,Frequencies)->IO ()
printFrecuencias (i,frec) = do
                          putStrLn ("Estadisticas de la pregunta " ++ (show i) ++ ": ")
                          putStrLn ("    Frecuencia absoluta aciertos: " ++ (show (fAbsRightNum frec)))
                          putStrLn ("    Frecuencia relativa aciertos: " ++ (show (fRelRightNum frec)))
                          putStrLn ("    Frecuencia absoluta fallos: " ++ (show (fAbsWrongNum frec)))
                          putStrLn ("    Frecuencia relativa fallos: " ++ (show (fRelWrongNum frec)))
                          putStrLn ("    Frecuencia absoluta blancos: " ++ (show (fAbsBlankNum frec)))
                          putStrLn ("    Frecuencia relativa blancos: " ++ (show (fRelBlankNum frec)))

--Getters Frecuencies
fAbsRightNum::Frequencies->FAbsRight
fAbsRightNum (a,_,_,_,_,_) = a

fRelRightNum::Frequencies->FRelRight
fRelRightNum (_,b,_,_,_,_) = b

fAbsWrongNum::Frequencies->FAbsWrong
fAbsWrongNum (_,_,c,_,_,_) = c

fRelWrongNum::Frequencies->FRelWrong
fRelWrongNum (_,_,_,d,_,_) = d

fAbsBlankNum::Frequencies->FAbsBlank
fAbsBlankNum (_,_,_,_,e,_) = e

fRelBlankNum::Frequencies->FRelBlank
fRelBlankNum (_,_,_,_,_,f) = f


--Getters de Estadisticas
getMedia::Estadisticas->MeanScore
getMedia (Estadisticas mean _ _ _ _ _ _ _ _ _ _) = mean

getMediaPreguntas::Estadisticas->MeanNumber
getMediaPreguntas (Estadisticas _ mNumber _ _ _ _ _ _ _ _ _) = mNumber

getSuspensos::Estadisticas->Suspensos
getSuspensos (Estadisticas _ _ susp _ _ _ _ _ _ _ _) = susp

getAprobados::Estadisticas->Aprobados
getAprobados (Estadisticas _ _ _ aprob _ _ _ _ _ _ _) = aprob

getNotables::Estadisticas->Notables
getNotables (Estadisticas _ _ _ _ not _ _ _ _ _ _) = not

getSobresalientes::Estadisticas->Sobresalientes
getSobresalientes (Estadisticas _ _ _ _ _ sobre _ _ _ _ _) = sobre

getListFrec::Estadisticas->[Frequencies]
getListFrec (Estadisticas _ _ _ _ _ _ lFrec _ _ _ _) = lFrec

getMejoresPreg::Estadisticas->MejoresPreguntas
getMejoresPreg (Estadisticas _ _ _ _ _ _ _ mPreg _ _ _) = mPreg

getPeoresPreg::Estadisticas->PeoresPreguntas
getPeoresPreg (Estadisticas _ _ _ _ _ _ _ _ pPreg _ _) = pPreg

getMasBlanco::Estadisticas->PreguntasMasBlanco
getMasBlanco (Estadisticas _ _ _ _ _ _ _ _ _ masBlanco _) = masBlanco

getMenosBlanco::Estadisticas->PreguntasMenosBlanco
getMenosBlanco (Estadisticas _ _ _ _ _ _ _ _ _ _ menosBlanco) = menosBlanco




---------------------------------------------------------------------------
--EJEMPLO DE EJECUCION SI QUIERES USARLO PARA PROBARLO
---------------------------------------------------------------------------

--Ejemplo Test
-- let x = Test [Question 3 [1,2], Question 4 [4,3], Question 3 [3,3], Question 3 [1,3], Question 4 [3,4], Question 3 [1,1],Question 4 [1,4], Question 5 [5,2]]

--Ejemplos respuestas
--let a = RespuestaTest 34278853 1 [2,0,1,2,2,0,4,5]
--let b = RespuestaTest 34278854 2 [0,1,3,0,4,0,2,2]
--let c = RespuestaTest 34278855 1 [1,0,2,2,1,0,1,0]
--let y = a:b:c:[]

--Despues con ellos llamar a las funciones pertinentes.
