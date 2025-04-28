from sppl.compilers.ast_to_spe import Id
from sppl.compilers.ast_to_spe import IfElse
from sppl.compilers.ast_to_spe import Otherwise
from sppl.compilers.ast_to_spe import Sample
from sppl.compilers.ast_to_spe import Sequence
from sppl.distributions import bernoulli
from sppl.distributions import uniform
from sppl.compilers.sppl_to_python import SPPL_Compiler

import argparse
import sys
            
compiler = SPPL_Compiler('''
a ~= uniform(loc=0, scale=1)
b ~= uniform(loc=0, scale=2)
c ~= uniform(loc=0, scale=3)
d ~= uniform(loc=0, scale=4)
e ~= uniform(loc=0, scale=5)
f ~= uniform(loc=0, scale=6)
g ~= uniform(loc=0, scale=7)
h ~= uniform(loc=0, scale=8)
i ~= uniform(loc=0, scale=9)
j ~= uniform(loc=0, scale=10)
k ~= uniform(loc=0, scale=11)
l ~= uniform(loc=0, scale=12)
m ~= uniform(loc=0, scale=13)
n ~= uniform(loc=0, scale=14)
o ~= uniform(loc=0, scale=15)
p ~= uniform(loc=0, scale=16)
q ~= uniform(loc=0, scale=17)
r ~= uniform(loc=0, scale=18)
s ~= uniform(loc=0, scale=19)
t ~= uniform(loc=0, scale=20)
u ~= uniform(loc=0, scale=21)
v ~= uniform(loc=0, scale=22)
w ~= uniform(loc=0, scale=23)
x ~= uniform(loc=0, scale=24)
y ~= uniform(loc=0, scale=25)
z ~= uniform(loc=0, scale=26)
a1 ~= uniform(loc=0, scale=27)
b1 ~= uniform(loc=0, scale=28)
c1 ~= uniform(loc=0, scale=29)
d1 ~= uniform(loc=0, scale=30)
e1 ~= uniform(loc=0, scale=31)
f1 ~= uniform(loc=0, scale=32)
g1 ~= uniform(loc=0, scale=33)
h1 ~= uniform(loc=0, scale=34)
i1 ~= uniform(loc=0, scale=35)
j1 ~= uniform(loc=0, scale=36)
k1 ~= uniform(loc=0, scale=37)
l1 ~= uniform(loc=0, scale=38)
m1 ~= uniform(loc=0, scale=39)
n1 ~= uniform(loc=0, scale=40)
o1 ~= uniform(loc=0, scale=41)
p1 ~= uniform(loc=0, scale=42)
q1 ~= uniform(loc=0, scale=43)
r1 ~= uniform(loc=0, scale=44)
s1 ~= uniform(loc=0, scale=45)
t1 ~= uniform(loc=0, scale=46)
u1 ~= uniform(loc=0, scale=47)
v1 ~= uniform(loc=0, scale=48)
w1 ~= uniform(loc=0, scale=49)
x1 ~= uniform(loc=0, scale=50)
y1 ~= uniform(loc=0, scale=51)

if (a < 0.5):
  if (b < 0.5):
    if (c < 0.5):
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    if (k < 0.5):
                      if (l < 0.5):
                        if (m < 0.5):
                          if (n < 0.5):
                            if (o < 0.5):
                              if (p < 0.5):
                                if (q < 0.5):
                                  if (r < 0.5):
                                    if (s < 0.5):
                                      if (t < 0.5):
                                        if (u < 0.5):
                                          if (v < 0.5):
                                            if (w < 0.5):
                                              if (x < 0.5):
                                                if (y < 0.5):
                                                  if (z < 0.5):
                                                    if (a1 < 0.5):
                                                      if (b1 < 0.5):
                                                        if (c1 < 0.5):
                                                          if (d1 < 0.5):
                                                            if (e1 < 0.5):
                                                              if (f1 < 0.5):
                                                                if (g1 < 0.5):
                                                                  if (h1 < 0.5):
                                                                    if (i1 < 0.5):
                                                                      if (j1 < 0.5):
                                                                        if (k1 < 0.5):
                                                                          if (l1 < 0.5):
                                                                            if (m1 < 0.5):
                                                                              if (n1 < 0.5):
                                                                                if (o1 < 0.5):
                                                                                  if (p1 < 0.5):
                                                                                    if (q1 < 0.5):
                                                                                      if (r1 < 0.5):
                                                                                        if (s1 < 0.5):
                                                                                          if (t1 < 0.5):
                                                                                            if (u1 < 0.5):
                                                                                              if (v1 < 0.5):
                                                                                                if (w1 < 0.5):
                                                                                                  if (x1 < 0.5):
                                                                                                    if (y1 < 0.5):
                                                                                                      z1 ~= uniform(loc=0, scale=1)
                                                                                                    else:
                                                                                                      z1 ~= uniform(loc=0, scale=2)
                                                                                                  else:
                                                                                                    z1 ~= uniform(loc=0, scale=3)
                                                                                                else:
                                                                                                  z1 ~= uniform(loc=0, scale=4)
                                                                                              else:
                                                                                                z1 ~= uniform(loc=0, scale=5)
                                                                                            else:
                                                                                              z1 ~= uniform(loc=0, scale=6)
                                                                                          else:
                                                                                            z1 ~= uniform(loc=0, scale=7)
                                                                                        else:
                                                                                          z1 ~= uniform(loc=0, scale=8)
                                                                                      else:
                                                                                        z1 ~= uniform(loc=0, scale=9)
                                                                                    else:
                                                                                      z1 ~= uniform(loc=0, scale=10)
                                                                                  else:
                                                                                    z1 ~= uniform(loc=0, scale=11)
                                                                                else:
                                                                                  z1 ~= uniform(loc=0, scale=12)
                                                                              else:
                                                                                z1 ~= uniform(loc=0, scale=13)
                                                                            else:
                                                                              z1 ~= uniform(loc=0, scale=14)
                                                                          else:
                                                                            z1 ~= uniform(loc=0, scale=15)
                                                                        else:
                                                                          z1 ~= uniform(loc=0, scale=16)
                                                                      else:
                                                                        z1 ~= uniform(loc=0, scale=17)
                                                                    else:
                                                                      z1 ~= uniform(loc=0, scale=18)
                                                                  else:
                                                                    z1 ~= uniform(loc=0, scale=19)
                                                                else:
                                                                  z1 ~= uniform(loc=0, scale=20)
                                                              else:
                                                                z1 ~= uniform(loc=0, scale=21)
                                                            else:
                                                              z1 ~= uniform(loc=0, scale=22)
                                                          else:
                                                            z1 ~= uniform(loc=0, scale=23)
                                                        else:
                                                          z1 ~= uniform(loc=0, scale=24)
                                                      else:
                                                        z1 ~= uniform(loc=0, scale=25)
                                                    else:
                                                      z1 ~= uniform(loc=0, scale=26)
                                                  else:
                                                    z1 ~= uniform(loc=0, scale=27)
                                                else:
                                                  z1 ~= uniform(loc=0, scale=28)
                                              else:
                                                z1 ~= uniform(loc=0, scale=29)
                                            else:
                                              z1 ~= uniform(loc=0, scale=30)
                                          else:
                                            z1 ~= uniform(loc=0, scale=31)
                                        else:
                                          z1 ~= uniform(loc=0, scale=32)
                                      else:
                                        z1 ~= uniform(loc=0, scale=33)
                                    else:
                                      z1 ~= uniform(loc=0, scale=34)
                                  else:
                                    z1 ~= uniform(loc=0, scale=35)
                                else:
                                  z1 ~= uniform(loc=0, scale=36)
                              else:
                                z1 ~= uniform(loc=0, scale=37)
                            else:
                              z1 ~= uniform(loc=0, scale=38)
                          else:
                            z1 ~= uniform(loc=0, scale=39)
                        else:
                          z1 ~= uniform(loc=0, scale=40)
                      else:
                        z1 ~= uniform(loc=0, scale=41)
                    else:
                      z1 ~= uniform(loc=0, scale=42)
                  else:
                    z1 ~= uniform(loc=0, scale=43)
                else:
                  z1 ~= uniform(loc=0, scale=44)
              else:
                z1 ~= uniform(loc=0, scale=45)
            else:
              z1 ~= uniform(loc=0, scale=46)
          else:
            z1 ~= uniform(loc=0, scale=47)
        else:
          z1 ~= uniform(loc=0, scale=48)
      else:
        z1 ~= uniform(loc=0, scale=49)
    else:
      z1 ~= uniform(loc=0, scale=50)
  else:
    z1 ~= uniform(loc=0, scale=51)
else:
  z1 ~= uniform(loc=0, scale=52)
                        ''')
namespace = compiler.execute_module()
z1      = Id('z1')
event = (z1 < 0.5)
print(namespace.model.prob(event))
    
