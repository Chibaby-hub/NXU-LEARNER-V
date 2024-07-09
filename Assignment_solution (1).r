generate_workers <- function() {
  workers <- list()
  names <- c("Joshua Akpan", "Victor Peter", "James John", "Victory Blessing", "Favour Bassey", "Joseph Israel")
  departments <- c("IT", "Sales", "Marketing", "Finance", "Human Resources", "Engineering", "Legal", "Customer Service")
  job_titles <- c("Sales Representative", "Sales Manager", "Sales Executive", "Sales Assistant", "Sales Coordinator", 
                  "Sales Consultant", "Sales Development Representative", "Sales Development Manager", 
                  "Sales Development Executive", "Sales Development Assistant", "Sales Development Coordinator", 
                  "Sales Development Consultant")

  for (num in 1:400) {
    tryCatch({
      name <- sample(names, 1)
      email <- paste0(gsub(" ", "", name), "@gmail.com")
      phone <- sample(1000000000:9999999999, 1)
      id <- num
      gender <- sample(c("Male", "Female"), 1)
      employment_level <- NA
      salary <- sample(2000:100000, 1)
      department <- sample(departments, 1)
      job_title <- sample(job_titles, 1)
      
      worker <- list(ID = id, Name = name, Email = email, Phone = phone, Gender = gender, 
                     Employment_Level = employment_level, Department = department, 
                     Job_Title = job_title, Salary = salary)
      workers[[length(workers) + 1]] <- worker
    }, error = function(e) {
      cat("Error while generating random attributes:", e$message, "\n")
    })
  }
  return(workers)
}

update_worker_details <- function(workers) {
  for (i in seq_along(workers)) {
    worker <- workers[[i]]
    tryCatch({
      if (worker$Salary > 10000 & worker$Salary < 20000) {
        worker$Employment_Level <- 'A1'
      } else if (worker$Salary > 7500 & worker$Salary < 30000 & worker$Gender == 'Female') {
        worker$Employment_Level <- 'A5-F'
      }
      workers[[i]] <- worker
    }, error = function(e) {
      cat("Error while assigning employment level to worker", worker$ID, ":", e$message, "\n")
    })
  }
}

print_workers <- function(workers) {
  for (worker in workers) {
    cat("Employee ID:", worker$ID, "\n",
        "Name:", worker$Name, "\n",
        "Email:", worker$Email, "\n",
        "Phone:", worker$Phone, "\n",
        "Gender:", worker$Gender, "\n",
        "Employment Level:", worker$Employment_Level, "\n",
        "Department:", worker$Department, "\n",
        "Job Title:", worker$Job_Title, "\n",
        "Salary:", worker$Salary, "\n\n")
  }
}

main <- function() {
  tryCatch({
    workers <- generate_workers()
  }, error = function(e) {
    cat("Error while generating workers:", e$message, "\n")
  })
  
  tryCatch({
    update_worker_details(workers)
  }, error = function(e) {
    cat("Error while updating workers:", e$message, "\n")
  })
  
  tryCatch({
    print_workers(workers)
  }, error = function(e) {
    cat("Error while printing workers:", e$message, "\n")
  })
}

main()