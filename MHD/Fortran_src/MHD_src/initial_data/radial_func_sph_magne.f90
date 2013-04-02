!
!      module radial_func_sph_magne
!
!      Written by H. Matsui
!
!      subroutine radial_function_sph(r, ifl, j_rst, r_min, r_max)
!
!      subroutine radial_function_sph_i(r,ifl,j_rst,r_min)
!      subroutine radial_function_sph_o(r,ifl,j_rst)
!      subroutine radial_function_sph_m(r,ifl,j_rst,r_max)
!
      module radial_func_sph_magne
!
      use m_precision
!
      use m_spherical_harmonics
!
      implicit none
!
      private :: radial_function_sph_i, radial_function_sph_o
      private :: radial_function_sph_m
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph(r, ifl, j_rst, r_min, r_max)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real (kind = kreal), intent(in) :: r, r_min, r_max
!
         if ( r  .lt. r_min ) then
          call radial_function_sph_i(r , ifl, j_rst, r_min)
         else if (  r  .gt. r_max ) then
          call radial_function_sph_m(r , ifl, j_rst, r_max)
         else
          call radial_function_sph_o(r , ifl, j_rst)
         end if
!
      end subroutine radial_function_sph
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_i(r,ifl,j_rst,r_min)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real (kind = kreal), intent(in) :: r, r_min
!
      integer(kind = kint) :: l_sp
!
!
      bp = 0.0d0
      bt = 0.0d0
      dbp = 0.0d0
!
      if ( j_rst.ne.0) then
!
        l_sp = int( aint(sqrt(dble(j_rst))) )
!
        bp(j_rst) =  sqrt(2.0) * (r/r_min)**(l_sp+1)                    &
     &         * cos( alpha(l_sp,ifl)*r_min - beta(l_sp,ifl) )
        dbp(j_rst) = sqrt(2.0) *dble(l_sp+1)*r**l_sp *r_min**(-l_sp-1)  &
     &         * cos( alpha(l_sp,ifl)*r_min - beta(l_sp,ifl) )
        mp(j_rst) = -sqrt(2.0) *dble(l_sp+1)*r**l_sp *r_min**(-l_sp-1)  &
     &         * cos( alpha(l_sp,ifl)*r_min - beta(l_sp,ifl) )
!
        bp(j_rst) = 0.1 *  bp(j_rst)
        dbp(j_rst) = 0.1 * dbp(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
!
      end if
!
      end subroutine radial_function_sph_i
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_o(r,ifl,j_rst)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real(kind = kreal), intent(in) :: r
!
      integer(kind = kint) :: l_sp
!
!
      if ( j_rst.ne.0) then
!
        l_sp = int( aint(sqrt(dble(j_rst))) )
!
        bp = 0.0d0
        bt = 0.0d0
        dbp = 0.0d0
!
        mp(j_rst) = 0.0d0
        bp(j_rst) =  sqrt(2.0)                                          &
     &      * cos( alpha(l_sp,ifl)* r - beta(l_sp,ifl) )
        dbp(j_rst) = - alpha(l_sp,ifl) *  sqrt(2.0)                     &
     &      * sin( alpha(l_sp,ifl)* r - beta(l_sp,ifl) )
!
        bp(j_rst) = 0.1 *  bp(j_rst)
        dbp(j_rst) = 0.1 *  dbp(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
!
      end if
!
      end subroutine radial_function_sph_o
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_m(r,ifl,j_rst,r_max)
!
      integer(kind = kint), intent(in) :: j_rst, ifl
      real(kind = kreal), intent(in) :: r, r_max
!
      integer(kind = kint) :: l_sp
!
!
        bp = 0.0d0
        bt = 0.0d0
        dbp = 0.0d0
!
      if ( j_rst.ne.0) then
!
        l_sp = int( aint(sqrt(dble(j_rst))) )
!
        bp(j_rst) =  sqrt(2.0) * (r_max/r)**(l_sp)                      &
     &          * cos( alpha(l_sp,ifl)*r_max - beta(l_sp,ifl) )
        dbp(j_rst) =-sqrt(2.0)*dble(l_sp) * r_max**l_sp * r**(-l_sp-1)  &
     &          * cos( alpha(l_sp,ifl)*r_max - beta(l_sp,ifl) )
        mp(j_rst) =  sqrt(2.0)*dble(l_sp) * r_max**l_sp * r**(-l_sp-1)  &
     &          * cos( alpha(l_sp,ifl)*r_max - beta(l_sp,ifl) )
!
        bp(j_rst) = 0.1 *  bp(j_rst)
        dbp(j_rst) = 0.1 * dbp(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
!
      end if
!
      end subroutine radial_function_sph_m
!
!-----------------------------------------------------------------------
!
      end module radial_func_sph_magne
