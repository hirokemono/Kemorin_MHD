!m_coef_fdm_fixed_ICB.f90
!      module m_coef_fdm_fixed_ICB
!
!     Written by H. Matsui on Jan., 2010
!
!      subroutine cal_2nd_nod_ICB_fixed_fdm
!      dfdr =      coef_fdm_fix_ICB_2(2,1) * d_nod(ICB  )
!                + coef_fdm_fix_ICB_2(2,2) * d_nod(ICB+1)
!                + coef_fdm_fix_ICB_2(2,3) * d_nod(ICB+2)
!      d2fdr2 =    coef_fdm_fix_ICB_2(3,1) * d_nod(ICB  )
!                + coef_fdm_fix_ICB_2(3,2) * d_nod(ICB+1)
!                + coef_fdm_fix_ICB_2(3,3) * d_nod(ICB+2)
!
!
!      subroutine cal_2nd_nod_ICB_fix_df_fdm
!
!      d2fdr2 =  mat_fdm_2(3,1) * dfdr(ICB)
!              + mat_fdm_2(3,2) * d_nod(ICB  )
!              + mat_fdm_2(3,3) * d_nod(ICB+1)
!
!
!      subroutine set_fixed_icb_fdm_mat_coefs
!      subroutine check_coef_fdm_fix_dr_ICB
!
      module m_coef_fdm_fixed_ICB
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use cal_inverse_small_matrix
!
      implicit none
!
!
      real(kind = kreal) :: coef_fdm_fix_ICB_2(0:2,3)
!      dfdr =      coef_fdm_fix_ICB_2( 0,2) * d_nod(ICB  )
!                + coef_fdm_fix_ICB_2( 1,2) * d_nod(ICB+1)
!                + coef_fdm_fix_ICB_2( 2,2) * d_nod(ICB+2)
!      d2fdr2 =    coef_fdm_fix_ICB_2( 0,3) * d_nod(ICB  )
!                + coef_fdm_fix_ICB_2( 1,3) * d_nod(ICB+1)
!                + coef_fdm_fix_ICB_2( 2,3) * d_nod(ICB+2)
!
!
      real(kind = kreal) :: coef_fdm_fix_dr_ICB_2(-1:1,3)
!      d_nod(k) =  coef_fdm_fix_dr_ICB_2(-1,2) * dfdr(ICB)
!                + coef_fdm_fix_dr_ICB_2( 0,2) * d_nod(ICB  )
!                + coef_fdm_fix_dr_ICB_2( 1,2) * d_nod(ICB+1)
!      d2fdr2 =    coef_fdm_fix_dr_ICB_2(-1,3) * dfdr(ICB)
!                + coef_fdm_fix_dr_ICB_2( 0,3) * d_nod(ICB  )
!                + coef_fdm_fix_dr_ICB_2( 1,3) * d_nod(ICB+1)
!
!
      real(kind = kreal), private :: mat_fdm_ICB_fix_2(3,3)
      real(kind = kreal), private :: mat_fdm_ICB_fix_dr_2(3,3)
      real(kind = kreal) :: mat_taylor_3(3,3)
!
      private :: mat_taylor_3
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_nod_ICB_fixed_fdm
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: dr_p1, dr_p2
!
!
      dr_p1 = radius_1d_rj_r(nlayer_ICB+1) - radius_1d_rj_r(nlayer_ICB)
      dr_p2 = radius_1d_rj_r(nlayer_ICB+2) - radius_1d_rj_r(nlayer_ICB)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = one
      mat_taylor_3(2,2) = dr_p1
      mat_taylor_3(2,3) = dr_p1*dr_p1 / two
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = dr_p2
      mat_taylor_3(3,3) = dr_p2*dr_p2 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ICB_fix_2, ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_2nd_nod_ICB_fixed_fdm ',        &
     &             nlayer_ICB, radius_1d_rj_r(nlayer_ICB)
      end if
!
      end subroutine cal_2nd_nod_ICB_fixed_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_nod_ICB_fix_df_fdm
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: dr_p1
!
!
        dr_p1 = dr_1d_rj(nlayer_ICB,0)
!
        mat_taylor_3(1,1) = one
        mat_taylor_3(1,2) = zero
        mat_taylor_3(1,3) = zero
!
        mat_taylor_3(2,1) = zero
        mat_taylor_3(2,2) = one
        mat_taylor_3(2,3) = zero
!
        mat_taylor_3(3,1) = dr_p1
        mat_taylor_3(3,2) = one
        mat_taylor_3(3,3) = dr_p1*dr_p1 / two
!
        call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ICB_fix_dr_2,  &
     &      ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_2nd_nod_ICB_fix_df_fdm ',       &
     &             nlayer_ICB, radius_1d_rj_r(nlayer_ICB)
      end if
!
      end subroutine cal_2nd_nod_ICB_fix_df_fdm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fixed_icb_fdm_mat_coefs
!
!
      coef_fdm_fix_ICB_2(0,1:3) = mat_fdm_ICB_fix_2(1:3,1)
      coef_fdm_fix_ICB_2(1,1:3) = mat_fdm_ICB_fix_2(1:3,2)
      coef_fdm_fix_ICB_2(2,1:3) = mat_fdm_ICB_fix_2(1:3,3)
!
      coef_fdm_fix_dr_ICB_2(-1,1:3) = mat_fdm_ICB_fix_dr_2(1:3,1)
      coef_fdm_fix_dr_ICB_2( 0,1:3) = mat_fdm_ICB_fix_dr_2(1:3,2)
      coef_fdm_fix_dr_ICB_2( 1,1:3) = mat_fdm_ICB_fix_dr_2(1:3,3)
!
      end subroutine set_fixed_icb_fdm_mat_coefs
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_fix_dr_ICB
!
!
      write(50,*) ' coef_fdm_fix_ICB_2'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_ICB_2(0:2,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_ICB_2(0:2,3)
!
      write(50,*) ' coef_fdm_fix_dr_ICB_2'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_dr_ICB_2(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_dr_ICB_2(-1:1,3)
!
      end subroutine check_coef_fdm_fix_dr_ICB
!
! -----------------------------------------------------------------------
!
      end module m_coef_fdm_fixed_ICB
