define p2dx_sonar8 ranger (
  scount 8

  # transducer pose [Xpos Ypos Heading]
  spose[0] [ 0.075 0.130 90 ]
  spose[1] [ 0.115 0.115 50 ]
  spose[2] [ 0.150 0.080 30 ]
  spose[3] [ 0.170 0.025 10 ]

  spose[4] [ 0.170 -0.025 -10 ]
  spose[5] [ 0.150 -0.080 -30 ]
  spose[6] [ 0.115 -0.115 -50 ]
  spose[7] [ 0.075 -0.130 -90 ]

  # transducer size in meters [X Y] (affects the Stage window)
  ssize[0] [ 0.01 0.05 ]
  ssize[1] [ 0.01 0.05 ]
  ssize[2] [ 0.01 0.05 ]
  ssize[3] [ 0.01 0.05 ]
  ssize[4] [ 0.01 0.05 ]
  ssize[5] [ 0.01 0.05 ]
  ssize[6] [ 0.01 0.05 ]
  ssize[7] [ 0.01 0.05 ]

  # transducer viewing parameters [Range_min Range_max FOV]
  sview[0] [ 0 5.0 15 ]
  sview[1] [ 0 5.0 15 ]
  sview[2] [ 0 5.0 15 ]
  sview[3] [ 0 5.0 15 ]
  sview[4] [ 0 5.0 15 ]
  sview[5] [ 0 5.0 15 ]
  sview[6] [ 0 5.0 15 ]
  sview[7] [ 0 5.0 15 ]
)

# ---[ standard Pioneer2 DX 16-sonars array ]---
define p2dx_sonar16 ranger (
  scount 16

  # transducer pose [Xpos Ypos Heading]
  spose[0] [ 0.075 0.130 90 ]
  spose[1] [ 0.115 0.115 50 ]
  spose[2] [ 0.150 0.080 30 ]
  spose[3] [ 0.170 0.025 10 ]

  spose[4] [ 0.170 -0.025 -10 ]
  spose[5] [ 0.150 -0.080 -30 ]
  spose[6] [ 0.115 -0.115 -50 ]
  spose[7] [ 0.075 -0.130 -90 ]

  spose[8] [ -0.155 -0.130 -90 ]
  spose[9] [ -0.195 -0.115 -130 ]
  spose[10] [ -0.230 -0.080 -150 ]
  spose[11] [ -0.250 -0.025 -170 ]

  spose[12] [ -0.250 0.025 170 ]
  spose[13] [ -0.230 0.080 150 ]
  spose[14] [ -0.195 0.115 130 ]
  spose[15] [ -0.155 0.130 90 ]

  # transducer size in meters [X Y] (affects the Stage window)
  ssize[0] [ 0.01 0.05 ]
  ssize[1] [ 0.01 0.05 ]
  ssize[2] [ 0.01 0.05 ]
  ssize[3] [ 0.01 0.05 ]
  ssize[4] [ 0.01 0.05 ]
  ssize[5] [ 0.01 0.05 ]
  ssize[6] [ 0.01 0.05 ]
  ssize[7] [ 0.01 0.05 ]
  ssize[8] [ 0.01 0.05 ]
  ssize[9] [ 0.01 0.05 ]
  ssize[10] [ 0.01 0.05 ]
  ssize[11] [ 0.01 0.05 ]
  ssize[12] [ 0.01 0.05 ]
  ssize[13] [ 0.01 0.05 ]
  ssize[14] [ 0.01 0.05 ]
  ssize[15] [ 0.01 0.05 ]

  # transducer viewing parameters [Range_min Range_max FOV]
  sview[0] [ 0 5.0 15 ]
  sview[1] [ 0 5.0 15 ]
  sview[2] [ 0 5.0 15 ]
  sview[3] [ 0 5.0 15 ]
  sview[4] [ 0 5.0 15 ]
  sview[5] [ 0 5.0 15 ]
  sview[6] [ 0 5.0 15 ]
  sview[7] [ 0 5.0 15 ]
  sview[8] [ 0 5.0 15 ]
  sview[9] [ 0 5.0 15 ]
  sview[10] [ 0 5.0 15 ]
  sview[11] [ 0 5.0 15 ]
  sview[12] [ 0 5.0 15 ]
  sview[13] [ 0 5.0 15 ]
  sview[14] [ 0 5.0 15 ]
  sview[15] [ 0 5.0 15 ]
)

# a Pioneer 2 or 3 in standard configuration
define pioneer2dx position
(
  # actual size
  size [0.44 0.33]

  # the pioneer's center of rotation is offset from its center of area
  origin [-0.04 0.0 0]

  # draw a nose on the robot so we can see which way it points
  gui_nose 1

  # estimated mass in KG
  mass 15.0

  # this polygon approximates the shape of a pioneer
  polygons 1
  polygon[0].points 8
  polygon[0].point[0] [  0.23  0.05 ]
  polygon[0].point[1] [  0.15  0.15 ]
  polygon[0].point[2] [ -0.15  0.15 ]
  polygon[0].point[3] [ -0.23  0.05 ]
  polygon[0].point[4] [ -0.23 -0.05 ]
  polygon[0].point[5] [ -0.15 -0.15 ]
  polygon[0].point[6] [  0.15 -0.15 ]
  polygon[0].point[7] [  0.23 -0.05 ]

  # differential steering model
  drive "diff"

  # uncomment this line if you want to model real pioneers with SICK
  # lasers, where the laser is taller than the robot
  # laser_return 0

  # use the sonar array defined above
  p2dx_sonar8()
)
