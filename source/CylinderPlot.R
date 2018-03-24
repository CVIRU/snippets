require(rgl)
open3d()
theta <- seq(0, 2*pi, len=25)
xx <- cbind(sin(theta)+2*sin(2*theta), 2*sin(3*theta), cos(theta)-2*cos(2*theta))
xx
plot3d(x = xx[, 1], y = xx[, 2], z = xx[, 3])
knot <- cylinder3d(cbind(sin(theta)+2*sin(2*theta), 2*sin(3*theta), cos(theta)-2*cos(2*theta)),
                   e1=cbind(cos(theta)+4*cos(2*theta), 6*cos(3*theta), sin(theta)+4*sin(2*theta)),
                   radius=0.8, closed=TRUE)
shade3d(addNormals(subdivision3d(knot, depth=2)), col="green") 

xx <- rbind(c(1,1,1),c(1,2,1))
xx
plot3d(x = xx[, 1], y = xx[, 2], z = xx[, 3])
knot <- cylinder3d(center = xx, closed = FALSE, debug  = FALSE)

cyl3d.2(center = xx, closed = FALSE, debug  = FALSE)


xx<- matrix(c(1,1,2,1,2,1,1,4,1),ncol=3)
xx
ellipse3d(xx)


data(mtcars)
fit <- lm(mpg ~ disp + cyl , mtcars)
fit
plot3d(ellipse3d(fit, level = .05), col="blue", alpha=0.5, aspect=TRUE)

vertices <- c( 
  -1.0, -1.0, 0, 1.0,
  1.0, -1.0, 0, 1.0,
  1.0,  1.0, 0, 1.0,
  -1.0,  1.0, 0, 1.0
)
indices <- c( 1, 2, 3, 4 )


wire3d( qmesh3d(vertices,indices) )


open3d()  
bg3d("gray")
l0 <- oh3d(tran = par3d("userMatrix"), color = "green" )
shade3d( translate3d( l0, -6, 0, 0 ))
l1 <- subdivision3d( l0 )
shade3d( translate3d( l1 , -2, 0, 0 ), color="red", override = FALSE )
l2 <- subdivision3d( l1 )
shade3d( translate3d( l2 , 2, 0, 0 ), color="red", override = TRUE )
l3 <- subdivision3d( l2 )
dot3d( translate3d( l3 , 6, 0, 0 ), color="red" )

# render all of the Platonic solids
open3d()
shade3d( translate3d( tetrahedron3d(col="red"), 0, 0, 0) )
shade3d( translate3d( cube3d(col="green"), 3, 0, 0) )
shade3d( translate3d( octahedron3d(col="blue"), 6, 0, 0) )
wire3d( translate3d( dodecahedron3d(col="cyan"), 9, 0, 0) )
wire3d( translate3d( icosahedron3d(col="magenta"), 12, 0, 0) )

library(help=rgl)
