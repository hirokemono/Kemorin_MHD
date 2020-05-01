!>@file   t_FastNoise.f90
!!@brief  module t_FastNoise
!!
!!@author H. Matsui
!!@date   Programmed  Jordan Peck in 2016
!!        Converted in Fortran by H. Matsui in Apr., 2020
!
!>@brief  Routines for FastNoise generator
!!
!!@verbatim
!!
!!static int FastFloor(float f) { return (f >= 0.0f ? (int)f : (int)f - 1); }
!!static int FastRound(float f) { return (f >= 0.0f) ? (int)(f + 0.5f) : (int)(f - 0.5f); }
!!static float FastAbs(float f) { return fabsf(f); }
!!static int FastAbs(int i) { return abs(i); }
!!static float Lerp(float a, float b, float t) { return a + t * (b - a); }
!!static float InterpHermiteFunc(float t) { return t*t*(3 - 2 * t); }
!!static float InterpQuinticFunc(float t) { return t*t*t*(t*(t * 6 - 15) + 10); }
!!static float CubicLerp(float a, float b, float c, float d, float t){
!!	float p = (d - c) - (a - b);
!!	return t * t * t * p + t * t * ((a - b) - p) + t * (c - a) + b;}
!!      integer(kind = kint) function FastFloor(f)
!!      integer(kind = kint) function FastRound(f)
!!      real(kind = kreal) function FastAbs(f)
!!      integer(kind = kint) function FastiAbs(i)
!!      real(kind = kreal) function Lerp(a, b, t)
!!      real(kind = kreal) function InterpHermiteFunc(t)
!!      real(kind = kreal) function InterpQuinticFunc(t)
!!      real(kind = kreal) function CubicLerp(a, b, c, d, t)
!!
!! MIT License
!!
!! Copyright(c) 2016 Jordan Peck
!!
!! Permission is hereby granted, free of charge, to any person obtaining a copy
!! of this software and associated documentation files(the "Software"), to deal
!! in the Software without restriction, including without limitation the rights
!! to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
!! copies of the Software, and to permit persons to whom the Software is
!! furnished to do so, subject to the following conditions :
!!
!! The above copyright notice and this permission notice shall be included in all
!! copies or substantial portions of the Software.
!!
!! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
!! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!! SOFTWARE.
!!
!! The developer's email is jorzixdan.me2@gzixmail.com (for great email, take
!! off every 'zix'.)
!!
!!@endverbatim
!
      module t_FastNoise
!
      use m_precision
      use m_constants
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function FastFloor(f)
!
      real(kind = kreal), intent(in) :: f
!
      if(f .ge. zero) then
        FastFloor = int(f)
      else
        FastFloor = int(f) - 1
      end if
!
      end function FastFloor
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function FastRound(f)
!
      real(kind = kreal), intent(in) :: f
!
      if(f .ge. zero) then
        FastRound = int(f + half)
      else
        FastRound = int(f - half)
      end if
!
      end function FastRound
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function FastAbs(f)
!
      real(kind = kreal), intent(in) :: f
!
        FastAbs = abs(f)
!
      end function FastAbs
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function FastiAbs(i)
!
      integer(kind = kint), intent(in) :: i
!
        FastiAbs = abs(i)
!
      end function FastiAbs
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function Lerp(a, b, t)
!
      real(kind = kreal), intent(in) :: a, b, t
!
        Lerp = a + t * (b - a)
!
      end function Lerp
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function InterpHermiteFunc(t)
!
      real(kind = kreal), intent(in) :: t
!
        InterpHermiteFunc = t*t*(3 - 2 * t)
!
      end function InterpHermiteFunc
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function InterpQuinticFunc(t)
!
      real(kind = kreal), intent(in) :: t
!
        InterpQuinticFunc = t*t*t*(t*(t * 6 - 15) + 10)
!
      end function InterpQuinticFunc
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function CubicLerp(a, b, c, d, t)
!
      real(kind = kreal), intent(in) :: a, b, c, d
      real(kind = kreal), intent(in) :: t
!
      real(kind = kreal) :: p
!
      p = (d - c) - (a - b)
      CubicLerp = t*t*t * p + t*t * ((a - b) - p) + t*(c - a) + b
!
      end function CubicLerp
!
! -----------------------------------------------------------------------
!
      end module t_FastNoise
