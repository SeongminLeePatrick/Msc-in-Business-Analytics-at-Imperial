data.new <- data
data.new$lag.quotations[1] <- 0
data.new$lag.orders[1] <- 0

data.new$cf.GRP.Radio <- 
  as.double(data.new$`GRP Radio` * coefficients(m4)["`GRP Radio`"])

data.new$cf.GRP.TV.Car1 <- 
  as.double(data.new$`GRP TV Car1` * coefficients(m4)["`GRP TV Car1`"])

data.new$cf.Other.Offline.Activities <- 
  as.double(data.new$`Other Offline Activities`* coefficients(m4)["`Other Offline Activities`"])

data.new$cf.Investment.On.Car1 <- 
  as.double(data.new$`Investment ON Car1` * coefficients(m4)["`Investment ON Car1`"])

data.new$cf.Investment.Search.Car1 <- 
  as.double(data.new$`Investment Search Car1` * coefficients(m4)["`Investment Search Car1`"])

data.new$cf.Investment.Programmatic.Car1 <- 
  as.double(data.new$`Investment Programmatic Car1` * coefficients(m4)["`Investment Programmatic Car1`"])

data.new$cf.Investment.TOT.UmbrellaBrand <- 
  as.double(data.new$`Investment TOT UmbrellaBrand` * coefficients(m4)["`Investment TOT UmbrellaBrand`"])

data.new$cf.Investment.TOT.UmbrellaBrand.OtherModels <- 
  as.double(data.new$`Investment TOT UmbrellaBrand OtherModels` * coefficients(m4)["`Investment TOT UmbrellaBrand OtherModels`"])

data.new$cf.Dummy.August <- 
  as.double(data.new$`Dummy August` * coefficients(m4)["`Dummy August`"])

data.new$cf.Dummy.Holidays <- 
  as.double(data.new$`Dummy Holidays` * coefficients(m4)["`Dummy Holidays`"])

data.new$cf.Dummy.NewCar1.Full.Launch <- 
  as.double(data.new$`Dummy NewCar1 Full Launch` * coefficients(m4)["`Dummy NewCar1 Full Launch`"])

data.new$cf.Dummy.OpenDoors.Car1 <- 
  as.double(data.new$`Dummy OpenDoors Car1` * coefficients(m4)["`Dummy OpenDoors Car1`"])

data.new$cf.Dummy.OpenDoors.Launch.NewCar1 <- 
  as.double(data.new$`Dummy OpenDoors Launch NewCar1` * coefficients(m4)["`Dummy OpenDoors Launch NewCar1`"])

data.new$cf.Dummy.RunOut.Car1 <- 
  as.double(data.new$`Dummy RunOut Car1` * coefficients(m4)["`Dummy RunOut Car1`"])

data.new$cf.lag.quotations <- 
  as.double(data.new$lag.quotations * coefficients(m4)["lag.quotations"])

data.new$cf.Investment.TOT.DirectCompetitor1.DirectCompetitor2 <- 
  as.double(data.new$`Investment TOT DirectCompetitor1+DirectCompetitor2` * 
              coefficients(m4)["`Investment TOT DirectCompetitor1+DirectCompetitor2`"])

data.new$cf.intercept <- as.double(coefficients(m4)["(Intercept)"])

data.new$cf.baseline <- 
  data.new$cf.Dummy.August + data.new$cf.Dummy.Holidays + data.new$cf.Dummy.NewCar1.Full.Launch +
  data.new$cf.Dummy.OpenDoors.Car1 + data.new$cf.Dummy.OpenDoors.Launch.NewCar1 + data.new$cf.Dummy.RunOut.Car1 + 
  data.new$cf.intercept + data.new$cf.lag.quotations + data.new$cf.Investment.TOT.DirectCompetitor1.DirectCompetitor2 + 
  data.new$cf.Investment.TOT.UmbrellaBrand + data.new$cf.Investment.On.Car1 + data.new$cf.Other.Offline.Activities

write.csv(data.new,'C:/Users/nirbh/Desktop/Coursework/03-Spring Term/04-Retail and Marketing Analytics/Assignments/Group/Group Assesment 02/Data_for_Decomposition_Graph.csv')
