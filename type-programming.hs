type FirstName = String
type LastName = String
type ISBN = String
type Title = String
type Band = String
type YearPublished = Int
type Price = Float

data Person = Person
{
    firstName :: String,
    lastName :: String
}

data Author = Author Person | Author [Person]

data Book = Book 
{
    authorName :: AuthorName, 
    title :: Title,
    year :: YearPublished,
    isbn :: ISBN,
    price :: Price
}

data Album = Album 
{
    band :: Band, 
    title :: Title,
    year :: YearPublished,
    price :: Price
}