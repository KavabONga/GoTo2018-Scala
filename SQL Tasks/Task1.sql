create table workers(
    id int,
    name varChar(20),
    age int,
    salary int,
    primary key (id)
)

insert into workers values(1, 'John', 23, 400), 
                          (2, 'Mary', 20, 450), 
                          (3, 'Max', 29, 1200), 
                          (4, 'Felix', 18, 350), 
                          (5, 'Groover', 22, 570),
                          (6, 'Ben', 19, 620)
select * from workers where id = 6
select * from workers where salary >= 500
insert into workers values (7, 'Denis', 23, 600)
delete from workers where name = 'Max'
select * from workers where salary <= 1000 and salary >= 500
