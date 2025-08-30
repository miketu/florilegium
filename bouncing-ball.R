# GGPlot based particle simulator by Michael Tu
# Inspiration and basic logic based on: https://doi.org/10.1007/978-1-4842-8185-7_4
# Last updated 2025-07-26
require(draw)

## Simulation Constants

step_size=0.1
simulation_time=2.0

## Particle Generator. Just copy and paste to add more particle objects
particles <- data.frame()


particles <- rbind(particles,
                   data.frame(radius=c(0.1),
                              position_x=c(2.5),position_y=c(2.5),
                              velocity_x=c(-3),velocity_y=c(0),
                              acceleration_x=c(0),acceleration_y=c(0))
                   )


particles <- rbind(particles,
                   data.frame(radius=c(0.1),
                              position_x=c(2.7),position_y=c(3.0),
                              velocity_x=c(3),velocity_y=c(0),
                              acceleration_x=c(0),acceleration_y=c(0)))

         




for(t in seq(0,simulation_time,by=step_size))
{
    drawSettings(pageWidth = 5, pageHeight = 5, units = "inches")
    drawPage()
    
     for(n_particles in 1:nrow(particles))
     {
         print(paste(n_particles," ",particles[n_particles,]$position_x," ",particles[n_particles,]$position_y))
         drawCircle(radius=particles[n_particles,]$radius,x=particles[n_particles,]$position_x,y=particles[n_particles,]$position_y,fill="red")
         
     }

    for(i in 1:nrow(particles))
    {
        
        particles[i,]$position_x=particles[i,]$position_x+particles[i,]$velocity_x*step_size
        particles[i,]$position_y=particles[i,]$position_y+particles[i,]$velocity_y*step_size
        particles[i,]$velocity_x=particles[i,]$velocity_x+particles[i,]$acceleration_x*step_size
        particles[i,]$velocity_y=particles[i,]$velocity_y+particles[i,]$acceleration_y*step_size
        
        
        
        # Wall Collision Feature
        if((abs(particles[i,]$position_x-5)<particles[i,]$radius))
         {
             particles[i,]$velocity_x=-1*particles[i,]$velocity_x
        }
        if((abs(particles[i,]$position_x+5)<particles[i,]$radius))
        {
            particles[i,]$velocity_x=-1*particles[i,]$velocity_x
        }
        
        if((abs(particles[i,]$position_y-5)<particles[i,]$radius))
        {
            particles[i,]$velocity_y=-1*particles[i,]$velocity_y
        }
        if((abs(particles[i,]$position_y+5)<particles[i,]$radius))
        {
            particles[i,]$velocity_y=-1*particles[i,]$velocity_y
        }
        
        
    }       
}
