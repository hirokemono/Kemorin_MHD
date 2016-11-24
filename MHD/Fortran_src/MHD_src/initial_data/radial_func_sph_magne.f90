!
!      module radial_func_sph_magne
!
!      Written by H. Matsui
!
!!      subroutine radial_function_sph(jmax, ifl, j_rst, l_rst,         &
!!     &          r, r_min, r_max, bp, bt, dbp, mp)
!
      module radial_func_sph_magne
!
      use m_precision
      use m_constants
      use m_initial_magne_coefs
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
      subroutine radial_function_sph(jmax, ifl, j_rst, l_rst,           &
     &          r, r_min, r_max, bp, bt, dbp, mp)
!
      integer(kind = kint), intent(in) :: jmax, j_rst, l_rst, ifl
      real (kind = kreal), intent(in) :: r, r_min, r_max
!
      real(kind = kreal), intent(inout) :: bp(0:jmax),  bt(0:jmax)
      real(kind = kreal), intent(inout) :: dbp(0:jmax), mp(0:jmax)
!
!
      if ( r  .lt. r_min ) then
        call radial_function_sph_i(jmax, ifl, j_rst, l_rst,             &
     &      r, r_min, bp, bt, dbp, mp)
      else if (  r  .gt. r_max ) then
        call radial_function_sph_m(jmax, ifl, j_rst, l_rst,             &
     &      r, r_max, bp, bt, dbp, mp)
      else
        call radial_function_sph_o                                      &
     &     (jmax, ifl, j_rst, l_rst, r, bp, bt, dbp, mp)
      end if
!
      end subroutine radial_function_sph
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_i(jmax, ifl, j_rst, l_rst,         &
     &          r, r_min, bp, bt, dbp, mp)
!
      integer(kind = kint), intent(in) :: jmax, j_rst, l_rst, ifl
      real (kind = kreal), intent(in) :: r, r_min
!
      real(kind = kreal), intent(inout) :: bp(0:jmax),  bt(0:jmax)
      real(kind = kreal), intent(inout) :: dbp(0:jmax), mp(0:jmax)
!
!
      bp = 0.0d0
      bt = 0.0d0
      dbp = 0.0d0
!
      if (j_rst .ne. 0) then
        bp(j_rst)                                                       &
     &       =  sqrt(2.0) * (r/r_min)**(l_rst+1)                        &
           &         * cos( alpha(l_rst,ifl)*r_min - beta(l_rst,ifl) )
        dbp(j_rst)                                                      &
     &       = sqrt(2.0) *dble(l_rst+1)*r**l_rst *r_min**(-l_rst-1)     &
     &         * cos( alpha(l_rst,ifl)*r_min - beta(l_rst,ifl) )
        mp(j_rst)                                                       &
     &       = -sqrt(2.0) *dble(l_rst+1)*r**l_rst *r_min**(-l_rst-1)    &
     &         * cos( alpha(l_rst,ifl)*r_min - beta(l_rst,ifl) )
!
        bp(j_rst) = 0.1 *  bp(j_rst)
        dbp(j_rst) = 0.1 * dbp(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
      end if
!
      end subroutine radial_function_sph_i
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_o                                  &
     &         (jmax, ifl, j_rst, l_rst, r, bp, bt, dbp, mp)
!
      integer(kind = kint), intent(in) :: jmax, j_rst, l_rst, ifl
      real(kind = kreal), intent(in) :: r
!
      real(kind = kreal), intent(inout) :: bp(0:jmax),  bt(0:jmax)
      real(kind = kreal), intent(inout) :: dbp(0:jmax), mp(0:jmax)
!
!
      if (j_rst .ne. 0) then
        bp = 0.0d0
        bt = 0.0d0
        dbp = 0.0d0
!
        mp(j_rst) = 0.0d0
        bp(j_rst) =  sqrt(2.0)                                          &
     &      * cos( alpha(l_rst,ifl)* r - beta(l_rst,ifl) )
        dbp(j_rst) = - alpha(l_rst,ifl) *  sqrt(2.0)                    &
     &      * sin( alpha(l_rst,ifl)* r - beta(l_rst,ifl) )
!
        bp(j_rst) = 0.1 *  bp(j_rst)
        dbp(j_rst) = 0.1 *  dbp(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
      end if
!
      end subroutine radial_function_sph_o
!
!-----------------------------------------------------------------------
!
      subroutine radial_function_sph_m(jmax, ifl, j_rst, l_rst,         &
     &          r, r_max, bp, bt, dbp, mp)
!
      integer(kind = kint), intent(in) :: jmax, j_rst, l_rst, ifl
      real(kind = kreal), intent(in) :: r, r_max
!
      real(kind = kreal), intent(inout) :: bp(0:jmax),  bt(0:jmax)
      real(kind = kreal), intent(inout) :: dbp(0:jmax), mp(0:jmax)
!
!
      bp = 0.0d0
      bt = 0.0d0
      dbp = 0.0d0
      mp = 0.0d0
      if(j_rst .ne. 0) then
        bp(j_rst)                                                       &
     &        =  sqrt(2.0) * (r_max/r)**(l_rst)                         &
     &          * cos( alpha(l_rst,ifl)*r_max - beta(l_rst,ifl) )
        dbp(j_rst)                                                      &
     &        = -sqrt(2.0)*dble(l_rst) * r_max**l_rst * r**(-l_rst-1)   &
     &          * cos( alpha(l_rst,ifl)*r_max - beta(l_rst,ifl) )
        mp(j_rst)                                                       &
     &        =  sqrt(2.0)*dble(l_rst) * r_max**l_rst * r**(-l_rst-1)   &
     &          * cos( alpha(l_rst,ifl)*r_max - beta(l_rst,ifl) )
!
        bp(j_rst) = 0.1 *  bp(j_rst)
        dbp(j_rst) = 0.1 * dbp(j_rst)
        mp(j_rst) = 0.1 *  mp(j_rst)
      end if
!
      end subroutine radial_function_sph_m
!
!-----------------------------------------------------------------------
!
      end module radial_func_sph_magne
