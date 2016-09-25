-- SQL 연습 문제 해답

select EmployeeID, count(*) n
from Orders
group by EmployeeID
order by n desc;


select a.EmployeeID, 
    b.FirstName, 
    b.LastName, 
    count(*) n
from Orders a
    inner join Employees b
        on a.EmployeeID = b.EmployeeID
group by a.EmployeeID
order by n desc;


select a.OrderID, 
    a.OrderDate, 
    sum(Quantity) as n_items, 
    sum(Quantity*c.Price) as total_price
from Orders a
    inner join OrderDetails b
        on a.OrderID = b.OrderID
    inner join Products c
        on b.ProductID = c.ProductID
group by a.OrderID;


