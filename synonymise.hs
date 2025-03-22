data Grade = Junior | Middle | Senior
data TypedPosition = Backend | Frontend | Analyst
data PositionData = PositionData Grade TypedPosition 
data Gender = Male | Female
data Name = 
  Name FirstName LastName | 
  NameWithPatronymic FirstName LastName Patronymic
data Employee = Employee Name Gender Age PositionData
data EmployeeNote = EmployeeNote {
  name :: Name,
  gender :: Gender,
  age :: Age,
  position :: PositionData
}
type Patronymic = String
type FirstName = String
type LastName = String
type EmployeeName = (FirstName, LastName)
type Age = Int
type Position = String

{- Функция employeeSummary, которая принимает на вход объект EmployeeNote и возвращает строку вида
***********
EmployeeName: Test, Testov
Gender: 
***********
-}

newTestEmployee = EmployeeNote {
  name = NameWithPatronymic "Oleg" "Eremichev" "Denisovich",
  age = 20,
  gender = Male,
  position = PositionData Junior Backend
}

testEmployeeWithPatronymic = Employee (NameWithPatronymic "Oleg" "Eremichev" "Denisovich")
  Male 20 (PositionData Junior Backend)

testEmployee = Employee (Name "" "")
  Male 20 (PositionData Junior Backend)

showPositionData :: PositionData -> String
showPositionData (PositionData g p) = showGrade g ++ " " ++ showTypedPosition p ++ " Developer"

showTypedPosition :: TypedPosition -> String
showTypedPosition Analyst = "Analyst"
showTypedPosition Frontend = "Frontend"
showTypedPosition Backend = "Backend"

showName :: Name -> String
showName (Name f l) = f ++ ", " ++ l
showName (NameWithPatronymic f l p) = l ++ " " ++ f ++ " " ++ p

showGender :: Gender -> String
showGender Male = "Male"
showGender Female = "Female"

showGrade :: Grade -> String
showGrade Junior = "Junior"
showGrade Middle = "Middle"
showGrade Senior = "Senior"

genderInitial :: Gender -> Char
genderInitial Male = 'M'
genderInitial Female = 'F'

getFirstName :: EmployeeName -> FirstName
getFirstName name = fst name
getLastName :: EmployeeName -> LastName
getLastName name = snd name

employeeInfo :: EmployeeNote -> String
employeeInfo employeeNote = 
  "*************" ++ "\n" ++
  "Employee: " ++ showName (name employeeNote) ++ "\n" ++
  "Gender: " ++ showGender (gender employeeNote) ++ "\n" ++
  "Age: " ++ show (age employeeNote) ++ "\n" ++
  "Position: " ++ showPositionData (position employeeNote) ++ "\n" ++
  "*************" ++ "\n"