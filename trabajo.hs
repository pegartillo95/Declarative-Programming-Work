--Test is RespuestaTest definiciones

--The easiest way to think Test is a list of Questions each of them holding the number of choices and the correct one that will be expresed as a RespuestasTest

--Test type
type NumChoices = Int
type CorrectChoice = Int
type ChoicesByModel = [CorrectChoice]
data Question = Question NumChoices ChoicesByModel deriving Show
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



--Getters
getQuestions::Test->QuestionList
getQuestions (Test questions) = questions

getDNI::RespuestaTest->DNI
getDNI (RespuestaTest dni _ _) = dni

getResponses::RespuestaTest->Responses
getResponses (RespuestaTest _ _ responses) = responses

getModel::RespuestaTest->TestModel
getModel (RespuestaTest _ testModel _) = testModel




--corrige::Test->RespuestaTest->Correccion
--corrige test responseTest = Correccion (getDNI responseTest) n (n/(length (getQuestions test)))
--                            where n = corrigeAux (getModel responseTest) (getQuestions test) (getResponses responseTest)

--corrigeAux::Num a=>TestModel->QuestionList->Responses->a
--corrigeAux _ _ _ = 0
