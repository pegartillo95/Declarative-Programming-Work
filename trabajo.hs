--Test is RespuestaTest definiciones

--The easiest way to think Test is a list of Questions each of them holding the number of choices and the correct one that will be expresed as a RespuestasTest

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
type CorrectAnswers = Float
type Grade = Float
data Correccion = Correccion DNI CorrectAnswers Grade deriving Show


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

--Other usefull functions

getNumQuestion::Test->Int
getNumQuestion (Test questions) = (length questions)

getNumQuestionFloat::Test->Float
getNumQuestionFloat (Test questions) = fromIntegral (length questions) :: Float


--Other functions
corrige::Test->RespuestaTest->Correccion
corrige test responseTest = Correccion (getDNI responseTest) n n --cambiar la primera n por m y hacer que de alguna manera corrige devuelva una tupla(Int,FLoat) en el primero el numero de respuestas correctas y en el segundo la nota
                            where n = corrigeAux (map getNumChoices (getQuestions test)) (map (correctForAModel (getNumQuestion test)) (getQuestions test)) (getResponses responseTest)

corrigeAux::[NumChoices]->[CorrectChoice]->Responses->Float
corrigeAux [] [] [] = 0.0
corrigeAux (x:xs) (y:ys) (z:zs)
      | y == z = corrigeAux xs ys zs + 1.0
      | otherwise = corrigeAux xs ys zs - (1.0 / ((fromIntegral x)::Float))


correctForAModel::Int->Question->CorrectChoice
correctForAModel n question  = (getCorrectByModel question)!!(n-1)
