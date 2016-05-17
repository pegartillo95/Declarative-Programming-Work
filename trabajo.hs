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

--Estadisticas data type
type MeanScore = Float
type MeanNumber = Int
type Suspensos = Int
type Aprobados = Int
type Notables = Int
type Sobresalientes = Int
data Estadisticas = Estadisticas MeanScore MeanNumber Suspensos Aprobados Notables Sobresalientes deriving Show


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

getCorrectAnswers::Correccion->Int
getCorrectAnswers (Correccion _ correctNum _) = correctNum


--Other usefull functions

getNumQuestion::Test->Int
getNumQuestion (Test questions) = (length questions)

getNumQuestionFloat::Test->Float
getNumQuestionFloat (Test questions) = fromIntegral (length questions) :: Float


--First part
corrige::Test->RespuestaTest->Correccion
corrige test responseTest = Correccion (getDNI responseTest) m n
                            where (m,n) = tupleWrapper (map getNumChoices (getQuestions test)) (map (correctForAModel (getModel responseTest)) (getQuestions test)) (getResponses responseTest) (getNumQuestion test)

tupleWrapper::[NumChoices]->[CorrectChoice]->Responses->Int->(Int,Float)
tupleWrapper numchoices correctchoices responses n = (countEqualsQuestions correctchoices responses ,corrigeAux numchoices correctchoices responses n)

countEqualsQuestions::[CorrectChoice]->Responses->Int
countEqualsQuestions [] _ = 0
countEqualsQuestions _ [] = 0
countEqualsQuestions (x:xs) (y:ys)
      | x == y = 1 + countEqualsQuestions xs ys
      | otherwise = countEqualsQuestions xs ys

corrigeAux::[NumChoices]->[CorrectChoice]->Responses->Int->Float
corrigeAux [] [] _ _ = 0.0
corrigeAux _ _ [] _ = 0.0
corrigeAux (x:xs) (y:ys) (z:zs) n
      | z == 0 = corrigeAux xs ys zs n
      | y == z = corrigeAux xs ys zs n + (10.0 / (fromIntegral n))
      | otherwise = corrigeAux xs ys zs n - (1.0 / (fromIntegral x))


correctForAModel::Int->Question->CorrectChoice
correctForAModel n question  = (getCorrectByModel question)!!(n-1)











--Second part
estadisticas::Test->[RespuestaTest]->Estadisticas
estadisticas test responsesTest = Estadisticas (mediaFloat (map getGrade resultList)) (mediaInt (map getCorrectAnswers resultList)) (numSuspensos resultList) (numAprobados resultList) (numNotables resultList) (numSobresalientes resultList)
                                  where 
                                       resultList = map (corrige test) responsesTest

mediaInt::[Int]-> Int
mediaInt x = sum x `div` length x

mediaFloat::[Float]->Float
mediaFloat x = sum x / (fromIntegral (length x))

numSuspensos::[Correccion]->Int
numSuspensos x = length(filter (<5) (map getGrade x))

numAprobados::[Correccion]->Int
numAprobados x = length( filter (<7) (filter (>=5) (map getGrade x)))

numNotables::[Correccion]->Int
numNotables x = length( filter (<9) (filter (>=7) (map getGrade x)))

numSobresalientes::[Correccion]->Int
numSobresalientes x = length(filter (>=9) (map getGrade x))