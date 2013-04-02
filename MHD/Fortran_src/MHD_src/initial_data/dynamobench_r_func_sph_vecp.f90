!dynamobench_r_func_sph_vecp.f90
!      module dynamobench_r_func_sph_vecp
!
!      Written by H. Matsui
!
!      subroutine radial_function_sph_vecp(r,ifl,j_rst,r_min)
!
!      subroutine radial_function_sph_vpi(r,ifl,j_rst,r_min)
!      subroutine radial_function_sph_vpo(r,ifl,j_rst)
!      subroutine radial_function_sph_vpm(r,ifl,j_rst,r_max)
!
      module dynamobench_r_func_sph_vecp
!
      use m_precision
!
      use m_spherical_harmonics
!
      implicit none
!
      private :: radial_function_sph_vpi, radial_function_sph_vpo
      private :: radial_function_sph_vpm
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_vecp(r, ifl, j_rst, r_min, r_max)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real (kind = kreal), intent(in) :: r, r_min, r_max
!
         if ( r  .lt. r_min ) then
          call radial_function_sph_vpi(r,ifl,j_rst,r_min)
         else if (  r .gt. r_max ) then
          call radial_function_sph_vpm(r,ifl,j_rst,r_max)
         else
          call radial_function_sph_vpo(r,ifl,j_rst)
         end if
!
      end subroutine radial_function_sph_vecp
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_vpi(r,ifl,j_rst,r_min)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real (kind = kreal), intent(in) :: r, r_min
!
      integer(kind = kint) :: l_sp
!
      bp = 0.0d0
      bt = 0.0d0
      dbp = 0.0d0
      mp = 0.0d0
!
      if ( j_rst.eq.0) then
!
        bt(2) =  (5.0d0 / 2.0d0) *( (20.0d0/13.0d0)                     &
     &            - (7.0d0/13.0d0) ) * r**2
!
        bp(6) = ap6_in * (r**3)
!
        dbp(6) = 3.0d0*ap6_in * (r**2)
!
      else
!
        l_sp = int( aint(sqrt(dble(j_rst))) )
!
        bt(j_rst) =  sqrt(2.0) * (r/r_min)**(l_sp+1)                    &
     &         * cos( alpha(l_sp,ifl)*r_min - beta(l_sp,ifl) )
        mp(j_rst) = -sqrt(2.0) *dble(l_sp+1)*r**l_sp *r_min**(-l_sp-1)  &
     &         * cos( alpha(l_sp,ifl)*r_min - beta(l_sp,ifl) )
!
        bt(j_rst) = 0.1 *  bt(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
!
      endif
!
      end subroutine radial_function_sph_vpi
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_vpo(r,ifl,j_rst)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real (kind = kreal), intent(in) :: r
!
      integer(kind = kint) :: l_sp, nn
!
!
      bp = 0.0d0
      bt = 0.0d0
      dbp = 0.0d0
      mp = 0.0d0
!
      if ( j_rst.eq.0) then
!
        bt(2) =  (5.0d0 / 8.0d0) *( -3.0d0*r**3                         &
     &          + 4.0d0*20.0d0*r**2/13.0d0 - (7.0d0/13.0d0)**4/r )
        do nn = 0, 10
          bp(6) = bp(6) + coef_ap(nn) * r**nn
        end do
!
        do nn = 1, 10
          dbp(6) = dbp(6) + dble(nn)*coef_ap(nn) * r**(nn-1)
        end do
!

      else
!
         l_sp = int( aint(sqrt(dble(j_rst))) )
!
         mp(j_rst) = 0.0d0
         bt(j_rst) =  sqrt(2.0)                                         &
     &        * cos( alpha(l_sp,ifl)* r - beta(l_sp,ifl) )
!
         bt(j_rst) = 0.1 *  bt(j_rst)
!
      end if
!
      end subroutine radial_function_sph_vpo
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_vpm(r,ifl,j_rst,r_max)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real (kind = kreal), intent(in) :: r, r_max
!
      integer(kind = kint) :: l_sp
!
      bp = 0.0d0
      bt = 0.0d0
      dbp = 0.0d0
      mp = 0.0d0
!
      if ( j_rst.eq.0) then
!
        bt(2) =  (5.0d0 / 8.0d0) *( (20.0d0/13.0d0)**4                  &
     &            - (7.0d0/13.0d0)**4 ) / r
!
        bp(6) = ap6_out / (r**2)
!
        dbp(6) = - 2.0d0*ap6_out / (r**3)
!
      else
!
        l_sp = int( aint(sqrt(dble(j_rst))) )
!
        bt(j_rst) =   sqrt(2.0) * (r_max/r)**(l_sp)                     &
     &          * cos( alpha(l_sp,ifl)*r_max - beta(l_sp,ifl) )
        mp(j_rst) =  0.0d0
!
        bt(j_rst) = 0.1 *  bt(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
!
      end if
!
      end subroutine radial_function_sph_vpm
!
!-----------------------------------------------------------------------
!
      end module dynamobench_r_func_sph_vecp
