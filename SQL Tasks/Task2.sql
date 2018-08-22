create table workers(
    id int,
    name varChar(20),
    age int,
    salary int,
    primary key (id)
)

create table experience(
    worker_id int,
    phone_number int,
    years_of_experience int
)

insert into workers values(1, 'John', 23, 400), 
                          (2, 'Mary', 20, 450), 
                          (3, 'Max', 29, 1200), 
                          (4, 'Felix', 18, 350), 
                          (5, 'Groover', 22, 570),
                          (6, 'Ben', 19, 620)
insert into experience values(1, 433443, 2), 
                             (2, 111111, 1), 
                             (3, 123456, 7), 
                             (4, 123568, 0), 
                             (5, 191919, 1),
                             (6, 121212, 1)
update experience set years_of_experience = 9 where worker_id = (select id from workers where name = 'Max')
select * from workers where (age >= 23 and age < 27) or salary = 1000
select * from workers where name = 'John' or name = 'Mary'
select phone_number from experience where worker_id = (select id from workers where name = 'John')
select * from workers where (select phone_number from experience where worker_id = id) % 10 = 1
select * from workers where (select years_of_experience from experience where worker_id = id) > 2
