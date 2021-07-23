

// calculate days between two dates

const calculateDays = (startDate, endDate) => {
  const start = new Date(startDate)
  const end = new Date(endDate)
  const diff = end - start
  const diffDays = Math.floor(diff / (1000 * 3600 * 24))
  return diffDays
}


// user service interface


