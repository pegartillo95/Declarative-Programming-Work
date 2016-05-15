--Test is RespuestaTest definiciones

--The easiest way to think Test is a list of Questions each of them holding the number of choices and the correct one that will be expresed as a RespuestasTest

--Test type
type TestModel = Int
type NumChoices = Int
type CorrectChoice = Int
data Question = Question NumChoices CorrectChoice deriving Show
type Test = [Question]


--RespuestasTest data type
type ChoiceMade = Int --We are going to consider 0 as a blank answer to a question from the student
type DNI = String
type Responses = [ChoiceMade]
data RespuestaTest = RespuestaTest DNI TestModel Responses 

corrige::Test->RespuestaTest->Correccion