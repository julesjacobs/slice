from sppl.compilers.ast_to_spe import Id
from sppl.compilers.ast_to_spe import IfElse
from sppl.compilers.ast_to_spe import Otherwise
from sppl.compilers.ast_to_spe import Sample
from sppl.compilers.ast_to_spe import Sequence
from sppl.distributions import bernoulli
from sppl.distributions import uniform
from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
a ~= uniform(loc=0,scale=1)
b ~= uniform(loc=0,scale=2)
c ~= uniform(loc=0,scale=3)
d ~= uniform(loc=0,scale=4)
e ~= uniform(loc=0,scale=5)
f ~= uniform(loc=0,scale=6)
g ~= uniform(loc=0,scale=7)
h ~= uniform(loc=0,scale=8)
i ~= uniform(loc=0,scale=9)
j ~= uniform(loc=0,scale=10)

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
                    k ~= uniform(loc=0,scale=1)
                  else:
                    k ~= uniform(loc=0,scale=2)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=3)
                  else:
                    k ~= uniform(loc=0,scale=4)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=5)
                  else:
                    k ~= uniform(loc=0,scale=6)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=7)
                  else:
                    k ~= uniform(loc=0,scale=8)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=9)
                  else:
                    k ~= uniform(loc=0,scale=10)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=11)
                  else:
                    k ~= uniform(loc=0,scale=12)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=13)
                  else:
                    k ~= uniform(loc=0,scale=14)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=15)
                  else:
                    k ~= uniform(loc=0,scale=16)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=17)
                  else:
                    k ~= uniform(loc=0,scale=18)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=19)
                  else:
                    k ~= uniform(loc=0,scale=20)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=21)
                  else:
                    k ~= uniform(loc=0,scale=22)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=23)
                  else:
                    k ~= uniform(loc=0,scale=24)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=25)
                  else:
                    k ~= uniform(loc=0,scale=26)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=27)
                  else:
                    k ~= uniform(loc=0,scale=28)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=29)
                  else:
                    k ~= uniform(loc=0,scale=30)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=31)
                  else:
                    k ~= uniform(loc=0,scale=32)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=33)
                  else:
                    k ~= uniform(loc=0,scale=34)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=35)
                  else:
                    k ~= uniform(loc=0,scale=36)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=37)
                  else:
                    k ~= uniform(loc=0,scale=38)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=39)
                  else:
                    k ~= uniform(loc=0,scale=40)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=41)
                  else:
                    k ~= uniform(loc=0,scale=42)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=43)
                  else:
                    k ~= uniform(loc=0,scale=44)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=45)
                  else:
                    k ~= uniform(loc=0,scale=46)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=47)
                  else:
                    k ~= uniform(loc=0,scale=48)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=49)
                  else:
                    k ~= uniform(loc=0,scale=50)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=51)
                  else:
                    k ~= uniform(loc=0,scale=52)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=53)
                  else:
                    k ~= uniform(loc=0,scale=54)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=55)
                  else:
                    k ~= uniform(loc=0,scale=56)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=57)
                  else:
                    k ~= uniform(loc=0,scale=58)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=59)
                  else:
                    k ~= uniform(loc=0,scale=60)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=61)
                  else:
                    k ~= uniform(loc=0,scale=62)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=63)
                  else:
                    k ~= uniform(loc=0,scale=64)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=65)
                  else:
                    k ~= uniform(loc=0,scale=66)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=67)
                  else:
                    k ~= uniform(loc=0,scale=68)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=69)
                  else:
                    k ~= uniform(loc=0,scale=70)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=71)
                  else:
                    k ~= uniform(loc=0,scale=72)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=73)
                  else:
                    k ~= uniform(loc=0,scale=74)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=75)
                  else:
                    k ~= uniform(loc=0,scale=76)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=77)
                  else:
                    k ~= uniform(loc=0,scale=78)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=79)
                  else:
                    k ~= uniform(loc=0,scale=80)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=81)
                  else:
                    k ~= uniform(loc=0,scale=82)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=83)
                  else:
                    k ~= uniform(loc=0,scale=84)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=85)
                  else:
                    k ~= uniform(loc=0,scale=86)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=87)
                  else:
                    k ~= uniform(loc=0,scale=88)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=89)
                  else:
                    k ~= uniform(loc=0,scale=90)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=91)
                  else:
                    k ~= uniform(loc=0,scale=92)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=93)
                  else:
                    k ~= uniform(loc=0,scale=94)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=95)
                  else:
                    k ~= uniform(loc=0,scale=96)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=97)
                  else:
                    k ~= uniform(loc=0,scale=98)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=99)
                  else:
                    k ~= uniform(loc=0,scale=100)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=101)
                  else:
                    k ~= uniform(loc=0,scale=102)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=103)
                  else:
                    k ~= uniform(loc=0,scale=104)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=105)
                  else:
                    k ~= uniform(loc=0,scale=106)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=107)
                  else:
                    k ~= uniform(loc=0,scale=108)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=109)
                  else:
                    k ~= uniform(loc=0,scale=110)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=111)
                  else:
                    k ~= uniform(loc=0,scale=112)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=113)
                  else:
                    k ~= uniform(loc=0,scale=114)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=115)
                  else:
                    k ~= uniform(loc=0,scale=116)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=117)
                  else:
                    k ~= uniform(loc=0,scale=118)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=119)
                  else:
                    k ~= uniform(loc=0,scale=120)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=121)
                  else:
                    k ~= uniform(loc=0,scale=122)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=123)
                  else:
                    k ~= uniform(loc=0,scale=124)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=125)
                  else:
                    k ~= uniform(loc=0,scale=126)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=127)
                  else:
                    k ~= uniform(loc=0,scale=128)
    else:
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=129)
                  else:
                    k ~= uniform(loc=0,scale=130)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=131)
                  else:
                    k ~= uniform(loc=0,scale=132)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=133)
                  else:
                    k ~= uniform(loc=0,scale=134)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=135)
                  else:
                    k ~= uniform(loc=0,scale=136)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=137)
                  else:
                    k ~= uniform(loc=0,scale=138)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=139)
                  else:
                    k ~= uniform(loc=0,scale=140)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=141)
                  else:
                    k ~= uniform(loc=0,scale=142)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=143)
                  else:
                    k ~= uniform(loc=0,scale=144)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=145)
                  else:
                    k ~= uniform(loc=0,scale=146)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=147)
                  else:
                    k ~= uniform(loc=0,scale=148)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=149)
                  else:
                    k ~= uniform(loc=0,scale=150)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=151)
                  else:
                    k ~= uniform(loc=0,scale=152)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=153)
                  else:
                    k ~= uniform(loc=0,scale=154)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=155)
                  else:
                    k ~= uniform(loc=0,scale=156)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=157)
                  else:
                    k ~= uniform(loc=0,scale=158)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=159)
                  else:
                    k ~= uniform(loc=0,scale=160)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=161)
                  else:
                    k ~= uniform(loc=0,scale=162)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=163)
                  else:
                    k ~= uniform(loc=0,scale=164)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=165)
                  else:
                    k ~= uniform(loc=0,scale=166)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=167)
                  else:
                    k ~= uniform(loc=0,scale=168)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=169)
                  else:
                    k ~= uniform(loc=0,scale=170)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=171)
                  else:
                    k ~= uniform(loc=0,scale=172)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=173)
                  else:
                    k ~= uniform(loc=0,scale=174)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=175)
                  else:
                    k ~= uniform(loc=0,scale=176)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=177)
                  else:
                    k ~= uniform(loc=0,scale=178)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=179)
                  else:
                    k ~= uniform(loc=0,scale=180)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=181)
                  else:
                    k ~= uniform(loc=0,scale=182)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=183)
                  else:
                    k ~= uniform(loc=0,scale=184)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=185)
                  else:
                    k ~= uniform(loc=0,scale=186)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=187)
                  else:
                    k ~= uniform(loc=0,scale=188)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=189)
                  else:
                    k ~= uniform(loc=0,scale=190)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=191)
                  else:
                    k ~= uniform(loc=0,scale=192)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=193)
                  else:
                    k ~= uniform(loc=0,scale=194)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=195)
                  else:
                    k ~= uniform(loc=0,scale=196)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=197)
                  else:
                    k ~= uniform(loc=0,scale=198)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=199)
                  else:
                    k ~= uniform(loc=0,scale=200)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=201)
                  else:
                    k ~= uniform(loc=0,scale=202)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=203)
                  else:
                    k ~= uniform(loc=0,scale=204)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=205)
                  else:
                    k ~= uniform(loc=0,scale=206)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=207)
                  else:
                    k ~= uniform(loc=0,scale=208)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=209)
                  else:
                    k ~= uniform(loc=0,scale=210)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=211)
                  else:
                    k ~= uniform(loc=0,scale=212)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=213)
                  else:
                    k ~= uniform(loc=0,scale=214)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=215)
                  else:
                    k ~= uniform(loc=0,scale=216)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=217)
                  else:
                    k ~= uniform(loc=0,scale=218)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=219)
                  else:
                    k ~= uniform(loc=0,scale=220)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=221)
                  else:
                    k ~= uniform(loc=0,scale=222)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=223)
                  else:
                    k ~= uniform(loc=0,scale=224)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=225)
                  else:
                    k ~= uniform(loc=0,scale=226)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=227)
                  else:
                    k ~= uniform(loc=0,scale=228)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=229)
                  else:
                    k ~= uniform(loc=0,scale=230)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=231)
                  else:
                    k ~= uniform(loc=0,scale=232)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=233)
                  else:
                    k ~= uniform(loc=0,scale=234)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=235)
                  else:
                    k ~= uniform(loc=0,scale=236)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=237)
                  else:
                    k ~= uniform(loc=0,scale=238)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=239)
                  else:
                    k ~= uniform(loc=0,scale=240)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=241)
                  else:
                    k ~= uniform(loc=0,scale=242)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=243)
                  else:
                    k ~= uniform(loc=0,scale=244)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=245)
                  else:
                    k ~= uniform(loc=0,scale=246)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=247)
                  else:
                    k ~= uniform(loc=0,scale=248)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=249)
                  else:
                    k ~= uniform(loc=0,scale=250)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=251)
                  else:
                    k ~= uniform(loc=0,scale=252)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=253)
                  else:
                    k ~= uniform(loc=0,scale=254)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=255)
                  else:
                    k ~= uniform(loc=0,scale=256)
  else:
    if (c < 0.5):
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=257)
                  else:
                    k ~= uniform(loc=0,scale=258)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=259)
                  else:
                    k ~= uniform(loc=0,scale=260)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=261)
                  else:
                    k ~= uniform(loc=0,scale=262)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=263)
                  else:
                    k ~= uniform(loc=0,scale=264)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=265)
                  else:
                    k ~= uniform(loc=0,scale=266)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=267)
                  else:
                    k ~= uniform(loc=0,scale=268)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=269)
                  else:
                    k ~= uniform(loc=0,scale=270)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=271)
                  else:
                    k ~= uniform(loc=0,scale=272)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=273)
                  else:
                    k ~= uniform(loc=0,scale=274)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=275)
                  else:
                    k ~= uniform(loc=0,scale=276)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=277)
                  else:
                    k ~= uniform(loc=0,scale=278)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=279)
                  else:
                    k ~= uniform(loc=0,scale=280)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=281)
                  else:
                    k ~= uniform(loc=0,scale=282)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=283)
                  else:
                    k ~= uniform(loc=0,scale=284)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=285)
                  else:
                    k ~= uniform(loc=0,scale=286)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=287)
                  else:
                    k ~= uniform(loc=0,scale=288)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=289)
                  else:
                    k ~= uniform(loc=0,scale=290)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=291)
                  else:
                    k ~= uniform(loc=0,scale=292)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=293)
                  else:
                    k ~= uniform(loc=0,scale=294)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=295)
                  else:
                    k ~= uniform(loc=0,scale=296)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=297)
                  else:
                    k ~= uniform(loc=0,scale=298)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=299)
                  else:
                    k ~= uniform(loc=0,scale=300)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=301)
                  else:
                    k ~= uniform(loc=0,scale=302)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=303)
                  else:
                    k ~= uniform(loc=0,scale=304)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=305)
                  else:
                    k ~= uniform(loc=0,scale=306)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=307)
                  else:
                    k ~= uniform(loc=0,scale=308)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=309)
                  else:
                    k ~= uniform(loc=0,scale=310)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=311)
                  else:
                    k ~= uniform(loc=0,scale=312)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=313)
                  else:
                    k ~= uniform(loc=0,scale=314)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=315)
                  else:
                    k ~= uniform(loc=0,scale=316)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=317)
                  else:
                    k ~= uniform(loc=0,scale=318)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=319)
                  else:
                    k ~= uniform(loc=0,scale=320)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=321)
                  else:
                    k ~= uniform(loc=0,scale=322)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=323)
                  else:
                    k ~= uniform(loc=0,scale=324)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=325)
                  else:
                    k ~= uniform(loc=0,scale=326)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=327)
                  else:
                    k ~= uniform(loc=0,scale=328)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=329)
                  else:
                    k ~= uniform(loc=0,scale=330)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=331)
                  else:
                    k ~= uniform(loc=0,scale=332)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=333)
                  else:
                    k ~= uniform(loc=0,scale=334)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=335)
                  else:
                    k ~= uniform(loc=0,scale=336)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=337)
                  else:
                    k ~= uniform(loc=0,scale=338)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=339)
                  else:
                    k ~= uniform(loc=0,scale=340)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=341)
                  else:
                    k ~= uniform(loc=0,scale=342)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=343)
                  else:
                    k ~= uniform(loc=0,scale=344)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=345)
                  else:
                    k ~= uniform(loc=0,scale=346)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=347)
                  else:
                    k ~= uniform(loc=0,scale=348)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=349)
                  else:
                    k ~= uniform(loc=0,scale=350)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=351)
                  else:
                    k ~= uniform(loc=0,scale=352)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=353)
                  else:
                    k ~= uniform(loc=0,scale=354)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=355)
                  else:
                    k ~= uniform(loc=0,scale=356)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=357)
                  else:
                    k ~= uniform(loc=0,scale=358)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=359)
                  else:
                    k ~= uniform(loc=0,scale=360)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=361)
                  else:
                    k ~= uniform(loc=0,scale=362)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=363)
                  else:
                    k ~= uniform(loc=0,scale=364)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=365)
                  else:
                    k ~= uniform(loc=0,scale=366)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=367)
                  else:
                    k ~= uniform(loc=0,scale=368)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=369)
                  else:
                    k ~= uniform(loc=0,scale=370)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=371)
                  else:
                    k ~= uniform(loc=0,scale=372)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=373)
                  else:
                    k ~= uniform(loc=0,scale=374)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=375)
                  else:
                    k ~= uniform(loc=0,scale=376)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=377)
                  else:
                    k ~= uniform(loc=0,scale=378)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=379)
                  else:
                    k ~= uniform(loc=0,scale=380)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=381)
                  else:
                    k ~= uniform(loc=0,scale=382)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=383)
                  else:
                    k ~= uniform(loc=0,scale=384)
    else:
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=385)
                  else:
                    k ~= uniform(loc=0,scale=386)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=387)
                  else:
                    k ~= uniform(loc=0,scale=388)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=389)
                  else:
                    k ~= uniform(loc=0,scale=390)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=391)
                  else:
                    k ~= uniform(loc=0,scale=392)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=393)
                  else:
                    k ~= uniform(loc=0,scale=394)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=395)
                  else:
                    k ~= uniform(loc=0,scale=396)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=397)
                  else:
                    k ~= uniform(loc=0,scale=398)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=399)
                  else:
                    k ~= uniform(loc=0,scale=400)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=401)
                  else:
                    k ~= uniform(loc=0,scale=402)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=403)
                  else:
                    k ~= uniform(loc=0,scale=404)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=405)
                  else:
                    k ~= uniform(loc=0,scale=406)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=407)
                  else:
                    k ~= uniform(loc=0,scale=408)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=409)
                  else:
                    k ~= uniform(loc=0,scale=410)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=411)
                  else:
                    k ~= uniform(loc=0,scale=412)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=413)
                  else:
                    k ~= uniform(loc=0,scale=414)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=415)
                  else:
                    k ~= uniform(loc=0,scale=416)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=417)
                  else:
                    k ~= uniform(loc=0,scale=418)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=419)
                  else:
                    k ~= uniform(loc=0,scale=420)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=421)
                  else:
                    k ~= uniform(loc=0,scale=422)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=423)
                  else:
                    k ~= uniform(loc=0,scale=424)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=425)
                  else:
                    k ~= uniform(loc=0,scale=426)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=427)
                  else:
                    k ~= uniform(loc=0,scale=428)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=429)
                  else:
                    k ~= uniform(loc=0,scale=430)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=431)
                  else:
                    k ~= uniform(loc=0,scale=432)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=433)
                  else:
                    k ~= uniform(loc=0,scale=434)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=435)
                  else:
                    k ~= uniform(loc=0,scale=436)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=437)
                  else:
                    k ~= uniform(loc=0,scale=438)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=439)
                  else:
                    k ~= uniform(loc=0,scale=440)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=441)
                  else:
                    k ~= uniform(loc=0,scale=442)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=443)
                  else:
                    k ~= uniform(loc=0,scale=444)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=445)
                  else:
                    k ~= uniform(loc=0,scale=446)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=447)
                  else:
                    k ~= uniform(loc=0,scale=448)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=449)
                  else:
                    k ~= uniform(loc=0,scale=450)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=451)
                  else:
                    k ~= uniform(loc=0,scale=452)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=453)
                  else:
                    k ~= uniform(loc=0,scale=454)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=455)
                  else:
                    k ~= uniform(loc=0,scale=456)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=457)
                  else:
                    k ~= uniform(loc=0,scale=458)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=459)
                  else:
                    k ~= uniform(loc=0,scale=460)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=461)
                  else:
                    k ~= uniform(loc=0,scale=462)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=463)
                  else:
                    k ~= uniform(loc=0,scale=464)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=465)
                  else:
                    k ~= uniform(loc=0,scale=466)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=467)
                  else:
                    k ~= uniform(loc=0,scale=468)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=469)
                  else:
                    k ~= uniform(loc=0,scale=470)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=471)
                  else:
                    k ~= uniform(loc=0,scale=472)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=473)
                  else:
                    k ~= uniform(loc=0,scale=474)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=475)
                  else:
                    k ~= uniform(loc=0,scale=476)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=477)
                  else:
                    k ~= uniform(loc=0,scale=478)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=479)
                  else:
                    k ~= uniform(loc=0,scale=480)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=481)
                  else:
                    k ~= uniform(loc=0,scale=482)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=483)
                  else:
                    k ~= uniform(loc=0,scale=484)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=485)
                  else:
                    k ~= uniform(loc=0,scale=486)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=487)
                  else:
                    k ~= uniform(loc=0,scale=488)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=489)
                  else:
                    k ~= uniform(loc=0,scale=490)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=491)
                  else:
                    k ~= uniform(loc=0,scale=492)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=493)
                  else:
                    k ~= uniform(loc=0,scale=494)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=495)
                  else:
                    k ~= uniform(loc=0,scale=496)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=497)
                  else:
                    k ~= uniform(loc=0,scale=498)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=499)
                  else:
                    k ~= uniform(loc=0,scale=500)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=501)
                  else:
                    k ~= uniform(loc=0,scale=502)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=503)
                  else:
                    k ~= uniform(loc=0,scale=504)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=505)
                  else:
                    k ~= uniform(loc=0,scale=506)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=507)
                  else:
                    k ~= uniform(loc=0,scale=508)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=509)
                  else:
                    k ~= uniform(loc=0,scale=510)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=511)
                  else:
                    k ~= uniform(loc=0,scale=512)
else:
  if (b < 0.5):
    if (c < 0.5):
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=513)
                  else:
                    k ~= uniform(loc=0,scale=514)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=515)
                  else:
                    k ~= uniform(loc=0,scale=516)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=517)
                  else:
                    k ~= uniform(loc=0,scale=518)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=519)
                  else:
                    k ~= uniform(loc=0,scale=520)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=521)
                  else:
                    k ~= uniform(loc=0,scale=522)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=523)
                  else:
                    k ~= uniform(loc=0,scale=524)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=525)
                  else:
                    k ~= uniform(loc=0,scale=526)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=527)
                  else:
                    k ~= uniform(loc=0,scale=528)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=529)
                  else:
                    k ~= uniform(loc=0,scale=530)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=531)
                  else:
                    k ~= uniform(loc=0,scale=532)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=533)
                  else:
                    k ~= uniform(loc=0,scale=534)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=535)
                  else:
                    k ~= uniform(loc=0,scale=536)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=537)
                  else:
                    k ~= uniform(loc=0,scale=538)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=539)
                  else:
                    k ~= uniform(loc=0,scale=540)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=541)
                  else:
                    k ~= uniform(loc=0,scale=542)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=543)
                  else:
                    k ~= uniform(loc=0,scale=544)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=545)
                  else:
                    k ~= uniform(loc=0,scale=546)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=547)
                  else:
                    k ~= uniform(loc=0,scale=548)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=549)
                  else:
                    k ~= uniform(loc=0,scale=550)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=551)
                  else:
                    k ~= uniform(loc=0,scale=552)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=553)
                  else:
                    k ~= uniform(loc=0,scale=554)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=555)
                  else:
                    k ~= uniform(loc=0,scale=556)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=557)
                  else:
                    k ~= uniform(loc=0,scale=558)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=559)
                  else:
                    k ~= uniform(loc=0,scale=560)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=561)
                  else:
                    k ~= uniform(loc=0,scale=562)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=563)
                  else:
                    k ~= uniform(loc=0,scale=564)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=565)
                  else:
                    k ~= uniform(loc=0,scale=566)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=567)
                  else:
                    k ~= uniform(loc=0,scale=568)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=569)
                  else:
                    k ~= uniform(loc=0,scale=570)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=571)
                  else:
                    k ~= uniform(loc=0,scale=572)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=573)
                  else:
                    k ~= uniform(loc=0,scale=574)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=575)
                  else:
                    k ~= uniform(loc=0,scale=576)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=577)
                  else:
                    k ~= uniform(loc=0,scale=578)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=579)
                  else:
                    k ~= uniform(loc=0,scale=580)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=581)
                  else:
                    k ~= uniform(loc=0,scale=582)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=583)
                  else:
                    k ~= uniform(loc=0,scale=584)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=585)
                  else:
                    k ~= uniform(loc=0,scale=586)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=587)
                  else:
                    k ~= uniform(loc=0,scale=588)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=589)
                  else:
                    k ~= uniform(loc=0,scale=590)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=591)
                  else:
                    k ~= uniform(loc=0,scale=592)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=593)
                  else:
                    k ~= uniform(loc=0,scale=594)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=595)
                  else:
                    k ~= uniform(loc=0,scale=596)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=597)
                  else:
                    k ~= uniform(loc=0,scale=598)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=599)
                  else:
                    k ~= uniform(loc=0,scale=600)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=601)
                  else:
                    k ~= uniform(loc=0,scale=602)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=603)
                  else:
                    k ~= uniform(loc=0,scale=604)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=605)
                  else:
                    k ~= uniform(loc=0,scale=606)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=607)
                  else:
                    k ~= uniform(loc=0,scale=608)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=609)
                  else:
                    k ~= uniform(loc=0,scale=610)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=611)
                  else:
                    k ~= uniform(loc=0,scale=612)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=613)
                  else:
                    k ~= uniform(loc=0,scale=614)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=615)
                  else:
                    k ~= uniform(loc=0,scale=616)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=617)
                  else:
                    k ~= uniform(loc=0,scale=618)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=619)
                  else:
                    k ~= uniform(loc=0,scale=620)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=621)
                  else:
                    k ~= uniform(loc=0,scale=622)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=623)
                  else:
                    k ~= uniform(loc=0,scale=624)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=625)
                  else:
                    k ~= uniform(loc=0,scale=626)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=627)
                  else:
                    k ~= uniform(loc=0,scale=628)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=629)
                  else:
                    k ~= uniform(loc=0,scale=630)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=631)
                  else:
                    k ~= uniform(loc=0,scale=632)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=633)
                  else:
                    k ~= uniform(loc=0,scale=634)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=635)
                  else:
                    k ~= uniform(loc=0,scale=636)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=637)
                  else:
                    k ~= uniform(loc=0,scale=638)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=639)
                  else:
                    k ~= uniform(loc=0,scale=640)
    else:
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=641)
                  else:
                    k ~= uniform(loc=0,scale=642)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=643)
                  else:
                    k ~= uniform(loc=0,scale=644)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=645)
                  else:
                    k ~= uniform(loc=0,scale=646)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=647)
                  else:
                    k ~= uniform(loc=0,scale=648)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=649)
                  else:
                    k ~= uniform(loc=0,scale=650)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=651)
                  else:
                    k ~= uniform(loc=0,scale=652)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=653)
                  else:
                    k ~= uniform(loc=0,scale=654)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=655)
                  else:
                    k ~= uniform(loc=0,scale=656)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=657)
                  else:
                    k ~= uniform(loc=0,scale=658)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=659)
                  else:
                    k ~= uniform(loc=0,scale=660)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=661)
                  else:
                    k ~= uniform(loc=0,scale=662)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=663)
                  else:
                    k ~= uniform(loc=0,scale=664)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=665)
                  else:
                    k ~= uniform(loc=0,scale=666)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=667)
                  else:
                    k ~= uniform(loc=0,scale=668)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=669)
                  else:
                    k ~= uniform(loc=0,scale=670)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=671)
                  else:
                    k ~= uniform(loc=0,scale=672)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=673)
                  else:
                    k ~= uniform(loc=0,scale=674)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=675)
                  else:
                    k ~= uniform(loc=0,scale=676)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=677)
                  else:
                    k ~= uniform(loc=0,scale=678)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=679)
                  else:
                    k ~= uniform(loc=0,scale=680)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=681)
                  else:
                    k ~= uniform(loc=0,scale=682)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=683)
                  else:
                    k ~= uniform(loc=0,scale=684)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=685)
                  else:
                    k ~= uniform(loc=0,scale=686)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=687)
                  else:
                    k ~= uniform(loc=0,scale=688)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=689)
                  else:
                    k ~= uniform(loc=0,scale=690)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=691)
                  else:
                    k ~= uniform(loc=0,scale=692)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=693)
                  else:
                    k ~= uniform(loc=0,scale=694)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=695)
                  else:
                    k ~= uniform(loc=0,scale=696)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=697)
                  else:
                    k ~= uniform(loc=0,scale=698)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=699)
                  else:
                    k ~= uniform(loc=0,scale=700)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=701)
                  else:
                    k ~= uniform(loc=0,scale=702)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=703)
                  else:
                    k ~= uniform(loc=0,scale=704)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=705)
                  else:
                    k ~= uniform(loc=0,scale=706)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=707)
                  else:
                    k ~= uniform(loc=0,scale=708)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=709)
                  else:
                    k ~= uniform(loc=0,scale=710)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=711)
                  else:
                    k ~= uniform(loc=0,scale=712)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=713)
                  else:
                    k ~= uniform(loc=0,scale=714)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=715)
                  else:
                    k ~= uniform(loc=0,scale=716)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=717)
                  else:
                    k ~= uniform(loc=0,scale=718)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=719)
                  else:
                    k ~= uniform(loc=0,scale=720)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=721)
                  else:
                    k ~= uniform(loc=0,scale=722)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=723)
                  else:
                    k ~= uniform(loc=0,scale=724)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=725)
                  else:
                    k ~= uniform(loc=0,scale=726)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=727)
                  else:
                    k ~= uniform(loc=0,scale=728)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=729)
                  else:
                    k ~= uniform(loc=0,scale=730)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=731)
                  else:
                    k ~= uniform(loc=0,scale=732)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=733)
                  else:
                    k ~= uniform(loc=0,scale=734)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=735)
                  else:
                    k ~= uniform(loc=0,scale=736)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=737)
                  else:
                    k ~= uniform(loc=0,scale=738)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=739)
                  else:
                    k ~= uniform(loc=0,scale=740)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=741)
                  else:
                    k ~= uniform(loc=0,scale=742)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=743)
                  else:
                    k ~= uniform(loc=0,scale=744)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=745)
                  else:
                    k ~= uniform(loc=0,scale=746)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=747)
                  else:
                    k ~= uniform(loc=0,scale=748)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=749)
                  else:
                    k ~= uniform(loc=0,scale=750)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=751)
                  else:
                    k ~= uniform(loc=0,scale=752)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=753)
                  else:
                    k ~= uniform(loc=0,scale=754)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=755)
                  else:
                    k ~= uniform(loc=0,scale=756)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=757)
                  else:
                    k ~= uniform(loc=0,scale=758)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=759)
                  else:
                    k ~= uniform(loc=0,scale=760)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=761)
                  else:
                    k ~= uniform(loc=0,scale=762)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=763)
                  else:
                    k ~= uniform(loc=0,scale=764)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=765)
                  else:
                    k ~= uniform(loc=0,scale=766)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=767)
                  else:
                    k ~= uniform(loc=0,scale=768)
  else:
    if (c < 0.5):
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=769)
                  else:
                    k ~= uniform(loc=0,scale=770)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=771)
                  else:
                    k ~= uniform(loc=0,scale=772)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=773)
                  else:
                    k ~= uniform(loc=0,scale=774)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=775)
                  else:
                    k ~= uniform(loc=0,scale=776)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=777)
                  else:
                    k ~= uniform(loc=0,scale=778)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=779)
                  else:
                    k ~= uniform(loc=0,scale=780)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=781)
                  else:
                    k ~= uniform(loc=0,scale=782)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=783)
                  else:
                    k ~= uniform(loc=0,scale=784)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=785)
                  else:
                    k ~= uniform(loc=0,scale=786)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=787)
                  else:
                    k ~= uniform(loc=0,scale=788)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=789)
                  else:
                    k ~= uniform(loc=0,scale=790)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=791)
                  else:
                    k ~= uniform(loc=0,scale=792)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=793)
                  else:
                    k ~= uniform(loc=0,scale=794)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=795)
                  else:
                    k ~= uniform(loc=0,scale=796)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=797)
                  else:
                    k ~= uniform(loc=0,scale=798)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=799)
                  else:
                    k ~= uniform(loc=0,scale=800)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=801)
                  else:
                    k ~= uniform(loc=0,scale=802)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=803)
                  else:
                    k ~= uniform(loc=0,scale=804)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=805)
                  else:
                    k ~= uniform(loc=0,scale=806)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=807)
                  else:
                    k ~= uniform(loc=0,scale=808)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=809)
                  else:
                    k ~= uniform(loc=0,scale=810)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=811)
                  else:
                    k ~= uniform(loc=0,scale=812)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=813)
                  else:
                    k ~= uniform(loc=0,scale=814)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=815)
                  else:
                    k ~= uniform(loc=0,scale=816)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=817)
                  else:
                    k ~= uniform(loc=0,scale=818)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=819)
                  else:
                    k ~= uniform(loc=0,scale=820)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=821)
                  else:
                    k ~= uniform(loc=0,scale=822)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=823)
                  else:
                    k ~= uniform(loc=0,scale=824)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=825)
                  else:
                    k ~= uniform(loc=0,scale=826)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=827)
                  else:
                    k ~= uniform(loc=0,scale=828)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=829)
                  else:
                    k ~= uniform(loc=0,scale=830)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=831)
                  else:
                    k ~= uniform(loc=0,scale=832)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=833)
                  else:
                    k ~= uniform(loc=0,scale=834)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=835)
                  else:
                    k ~= uniform(loc=0,scale=836)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=837)
                  else:
                    k ~= uniform(loc=0,scale=838)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=839)
                  else:
                    k ~= uniform(loc=0,scale=840)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=841)
                  else:
                    k ~= uniform(loc=0,scale=842)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=843)
                  else:
                    k ~= uniform(loc=0,scale=844)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=845)
                  else:
                    k ~= uniform(loc=0,scale=846)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=847)
                  else:
                    k ~= uniform(loc=0,scale=848)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=849)
                  else:
                    k ~= uniform(loc=0,scale=850)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=851)
                  else:
                    k ~= uniform(loc=0,scale=852)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=853)
                  else:
                    k ~= uniform(loc=0,scale=854)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=855)
                  else:
                    k ~= uniform(loc=0,scale=856)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=857)
                  else:
                    k ~= uniform(loc=0,scale=858)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=859)
                  else:
                    k ~= uniform(loc=0,scale=860)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=861)
                  else:
                    k ~= uniform(loc=0,scale=862)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=863)
                  else:
                    k ~= uniform(loc=0,scale=864)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=865)
                  else:
                    k ~= uniform(loc=0,scale=866)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=867)
                  else:
                    k ~= uniform(loc=0,scale=868)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=869)
                  else:
                    k ~= uniform(loc=0,scale=870)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=871)
                  else:
                    k ~= uniform(loc=0,scale=872)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=873)
                  else:
                    k ~= uniform(loc=0,scale=874)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=875)
                  else:
                    k ~= uniform(loc=0,scale=876)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=877)
                  else:
                    k ~= uniform(loc=0,scale=878)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=879)
                  else:
                    k ~= uniform(loc=0,scale=880)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=881)
                  else:
                    k ~= uniform(loc=0,scale=882)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=883)
                  else:
                    k ~= uniform(loc=0,scale=884)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=885)
                  else:
                    k ~= uniform(loc=0,scale=886)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=887)
                  else:
                    k ~= uniform(loc=0,scale=888)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=889)
                  else:
                    k ~= uniform(loc=0,scale=890)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=891)
                  else:
                    k ~= uniform(loc=0,scale=892)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=893)
                  else:
                    k ~= uniform(loc=0,scale=894)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=895)
                  else:
                    k ~= uniform(loc=0,scale=896)
    else:
      if (d < 0.5):
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=897)
                  else:
                    k ~= uniform(loc=0,scale=898)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=899)
                  else:
                    k ~= uniform(loc=0,scale=900)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=901)
                  else:
                    k ~= uniform(loc=0,scale=902)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=903)
                  else:
                    k ~= uniform(loc=0,scale=904)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=905)
                  else:
                    k ~= uniform(loc=0,scale=906)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=907)
                  else:
                    k ~= uniform(loc=0,scale=908)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=909)
                  else:
                    k ~= uniform(loc=0,scale=910)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=911)
                  else:
                    k ~= uniform(loc=0,scale=912)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=913)
                  else:
                    k ~= uniform(loc=0,scale=914)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=915)
                  else:
                    k ~= uniform(loc=0,scale=916)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=917)
                  else:
                    k ~= uniform(loc=0,scale=918)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=919)
                  else:
                    k ~= uniform(loc=0,scale=920)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=921)
                  else:
                    k ~= uniform(loc=0,scale=922)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=923)
                  else:
                    k ~= uniform(loc=0,scale=924)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=925)
                  else:
                    k ~= uniform(loc=0,scale=926)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=927)
                  else:
                    k ~= uniform(loc=0,scale=928)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=929)
                  else:
                    k ~= uniform(loc=0,scale=930)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=931)
                  else:
                    k ~= uniform(loc=0,scale=932)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=933)
                  else:
                    k ~= uniform(loc=0,scale=934)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=935)
                  else:
                    k ~= uniform(loc=0,scale=936)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=937)
                  else:
                    k ~= uniform(loc=0,scale=938)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=939)
                  else:
                    k ~= uniform(loc=0,scale=940)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=941)
                  else:
                    k ~= uniform(loc=0,scale=942)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=943)
                  else:
                    k ~= uniform(loc=0,scale=944)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=945)
                  else:
                    k ~= uniform(loc=0,scale=946)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=947)
                  else:
                    k ~= uniform(loc=0,scale=948)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=949)
                  else:
                    k ~= uniform(loc=0,scale=950)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=951)
                  else:
                    k ~= uniform(loc=0,scale=952)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=953)
                  else:
                    k ~= uniform(loc=0,scale=954)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=955)
                  else:
                    k ~= uniform(loc=0,scale=956)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=957)
                  else:
                    k ~= uniform(loc=0,scale=958)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=959)
                  else:
                    k ~= uniform(loc=0,scale=960)
      else:
        if (e < 0.5):
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=961)
                  else:
                    k ~= uniform(loc=0,scale=962)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=963)
                  else:
                    k ~= uniform(loc=0,scale=964)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=965)
                  else:
                    k ~= uniform(loc=0,scale=966)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=967)
                  else:
                    k ~= uniform(loc=0,scale=968)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=969)
                  else:
                    k ~= uniform(loc=0,scale=970)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=971)
                  else:
                    k ~= uniform(loc=0,scale=972)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=973)
                  else:
                    k ~= uniform(loc=0,scale=974)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=975)
                  else:
                    k ~= uniform(loc=0,scale=976)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=977)
                  else:
                    k ~= uniform(loc=0,scale=978)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=979)
                  else:
                    k ~= uniform(loc=0,scale=980)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=981)
                  else:
                    k ~= uniform(loc=0,scale=982)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=983)
                  else:
                    k ~= uniform(loc=0,scale=984)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=985)
                  else:
                    k ~= uniform(loc=0,scale=986)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=987)
                  else:
                    k ~= uniform(loc=0,scale=988)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=989)
                  else:
                    k ~= uniform(loc=0,scale=990)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=991)
                  else:
                    k ~= uniform(loc=0,scale=992)
        else:
          if (f < 0.5):
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=993)
                  else:
                    k ~= uniform(loc=0,scale=994)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=995)
                  else:
                    k ~= uniform(loc=0,scale=996)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=997)
                  else:
                    k ~= uniform(loc=0,scale=998)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=999)
                  else:
                    k ~= uniform(loc=0,scale=1000)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1001)
                  else:
                    k ~= uniform(loc=0,scale=1002)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1003)
                  else:
                    k ~= uniform(loc=0,scale=1004)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1005)
                  else:
                    k ~= uniform(loc=0,scale=1006)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1007)
                  else:
                    k ~= uniform(loc=0,scale=1008)
          else:
            if (g < 0.5):
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1009)
                  else:
                    k ~= uniform(loc=0,scale=1010)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1011)
                  else:
                    k ~= uniform(loc=0,scale=1012)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1013)
                  else:
                    k ~= uniform(loc=0,scale=1014)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1015)
                  else:
                    k ~= uniform(loc=0,scale=1016)
            else:
              if (h < 0.5):
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1017)
                  else:
                    k ~= uniform(loc=0,scale=1018)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1019)
                  else:
                    k ~= uniform(loc=0,scale=1020)
              else:
                if (i < 0.5):
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1021)
                  else:
                    k ~= uniform(loc=0,scale=1022)
                else:
                  if (j < 0.5):
                    k ~= uniform(loc=0,scale=1023)
                  else:
                    k ~= uniform(loc=0,scale=1024)
                         ''')
namespace = compiler.execute_module()
k      = Id('k')
event = (k < 0.5)
print(namespace.model.prob(event))