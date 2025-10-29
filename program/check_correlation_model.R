# Simple check for correlation model
cat("=== CHECKING CORRELATION MODEL ===\n")

# Load the file
load("output/10c_starima_correlation.RData")

# List all objects
cat("All objects in file:\n")
print(ls())

# Check each object
for (obj_name in ls()) {
  obj <- get(obj_name)
  cat("\n", obj_name, ":\n")
  cat("Class:", class(obj), "\n")
  
  if (is.list(obj)) {
    cat("List elements:", names(obj), "\n")
    
    if ("phi" %in% names(obj)) {
      cat("Phi coefficients:\n")
      print(obj$phi)
    }
    
    if ("theta" %in% names(obj)) {
      cat("Theta coefficients:\n") 
      print(obj$theta)
    }
  }
}

cat("\n=== END CHECK ===\n")