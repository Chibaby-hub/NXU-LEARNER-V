import random

def generate_workers():

    workers = []

    for num in range(1, 401):
        # Name
        # Email
        # Phone Number
        # Employment ID
        # Gender
        # Employment Level
        # Salary
        # Department
        # Job Title
        try:
            name = random.choice(["Joshua Akpan", "Victor Peter", "James John", "Victory Blessing", "Favour Bassey", "Joseph Israel"])
            email = name.replace(" ", "") + "@gmail.com"
            Phone = random.randint(1000000000, 9999999999)
            ID = num
            Gender = random.choice(["Male", "Female"])
            Employment_Level = None
            Salary = random.randint(2000, 100000)
            Department = random.choice(["IT", "Sales", "Marketing", "Finance", "Human Resources", "Engineering", "Legal", "Customer Service"])
            Job_Title = random.choice(["Sales Representative", "Sales Manager", "Sales Executive", "Sales Assistant", "Sales Coordinator", "Sales Consultant", "Sales Development Representative", "Sales Development Manager", "Sales Development Executive", "Sales Development Assistant", "Sales Development Coordinator", "Sales Development Consultant", "Sales Development Representative", "Sales Development Manager", "Sales Development Executive", "Sales Development Assistant", "Sales Development Coordinator", "Sales Development Consultant", "Sales Development Representative", "Sales Development Manager", "Sales Development Executive", "Sales Development Assistant", "Sales Development Coordinator", "Sales Development Consultant", "Sales Development Representative", "Sales Development Manager", "Sales Development Executive", "Sales Development Assistant", "Sales Development Coordinator", "Sales Development Consultant", "Sales Development Representative", "Sales Development Manager", "Sales Development Executive", "Sales Development Assistant", "Sales Development Coordinator", "Sales Development Consultant", "Sales Development Representative", "Sales Development Manager", "Sales Development Executive"])
        except Exception as e:
            print(f"Error while generating random attributes: {e}")
                
        worker = [ID, name, email, Phone, Gender, Employment_Level, Department, Job_Title, Salary]
        try:
            workers.append(worker)
        except Exception as e:
            print(f"Error while appending worker {num}: {e}")
            
    return workers
            
def update_worker_details(workers):   
    for worker in workers:
        try:
            if worker[-1] > 10000 and worker[-1] < 20000:
                worker[5] = 'A1'
            elif worker[-1] > 7500 and worker[-1] < 30000 and worker[4] == 'Female':
                worker[5] = 'A5-F'
        except Exception as e:
            print(f"Error while assigning employment level to worker {worker[0]}: {e}")

def print_workers(workers):
    for worker in workers :
        print(f"Employee ID: {worker[0]},\n \
            Name: {worker[1]},\n \
            Email: {worker[2]},\n \
            Phone: {worker[3]},\n \
            Gender: {worker[4]},\n \
            Employment Level: {worker[5]},\n \
            Department: {worker[6]},\n \
            Job Title: {worker[7]},\n \
            Salary: {worker[8]}\n")

def main():
    try:
        workers = generate_workers()
    except Exception as e:
        print(f"Error while generating workers: {e}")
    
    try:
        update_worker_details(workers)
    except Exception as e:
        print(f"Error while updating workers: {e}")
        
    try:
        print_workers(workers)
    except Exception as e:
        print(f"Error while printing workers: {e}")
        
if __name__ == "__main__":
    main()