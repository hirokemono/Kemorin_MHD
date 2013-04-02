!m_coef_fdm_free_CMB.f90
!      module m_coef_fdm_free_CMB
!
!     Written by H. Matsui on Jan., 2010
!
!      subroutine cal_2nd_nod_CMB_free_bc_fdm
!      dsdr =    mat_fdm_CMB_free_vp(2,1) * d_nod(ICB  )
!              + mat_fdm_CMB_free_vp(2,3) * d_nod(ICB+1)
!      dsfdr2 =  mat_fdm_CMB_free_vp(3,1) * d_nod(ICB  )
!              + mat_fdm_CMB_free_vp(3,3) * d_nod(ICB+1)
!
!      dtdr =    mat_fdm_CMB_free_vt(2,1) * d_nod(ICB  )
!              + mat_fdm_CMB_free_vt(2,3) * d_nod(ICB+1)
!      dtfdr2 =  mat_fdm_CMB_free_vt(3,1) * d_nod(ICB  )
!              + mat_fdm_CMB_free_vt(3,3) * d_nod(ICB+1)
!
!      dfdr =    mat_fdm_2(2,1) * d_nod(CMB  )
!              + mat_fdm_2(2,2)
!                 * (-2*dfdr(CMB) + r(CMB) * d2fdr2(CMB))
!              + mat_fdm_2(2,3) * d_nod(CMB-1)
!      d2fdr2 =  mat_fdm_2(3,1) * d_nod(CMB  )
!              + mat_fdm_2(3,2)
!                 * (-2*dfdr(CMB) + r(CMB) * d2fdr2(CMB))
!              + mat_fdm_2(3,3) * d_nod(CMB-1)
!
!
!      subroutine set_free_cmb_fdm_mat_coefs
!      subroutine check_coef_fdm_free_CMB
!
      module m_coef_fdm_free_CMB
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use cal_inverse_small_matrix
!
      implicit none
!
      real(kind = kreal) :: coef_fdm_free_CMB_vp2(-1:0,3)
!      dfdr =    coef_fdm_free_CMB_vp2(-1,2) * d_nod(CMB-1)
!              + coef_fdm_free_CMB_vp2( 0,2) * d_nod(CMB  )
!      d2fdr2 =  coef_fdm_free_CMB_vp2(-1,3) * d_nod(CMB-1)
!              + coef_fdm_free_CMB_vp2( 0,3) * d_nod(CMB  )
!
      real(kind = kreal) :: coef_fdm_free_CMB_vt2(-1:0,3)
!      dfdr =    coef_fdm_free_CMB_vt2( 0,2) * d_nod(CMB  )
!      d2fdr2 =  coef_fdm_free_CMB_vt2(-1,3) * d_nod(CMB-1)
!              + coef_fdm_free_CMB_vt2( 0,3) * d_nod(CMB  )
!
!
      real(kind = kreal), private :: mat_fdm_CMB_free_vp(3,3)
      real(kind = kreal), private :: mat_fdm_CMB_free_vt(3,3)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_nod_CMB_free_bc_fdm
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_n1, r0, r1
!
!
      dr_n1 = dr_1d_rj(nlayer_CMB,1)
      r0 = radius_1d_rj_r(nlayer_CMB  )
      r1 = radius_1d_rj_r(nlayer_CMB-1)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = zero
      mat_taylor_3(2,2) = one
      mat_taylor_3(2,3) = zero
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = zero
      mat_taylor_3(3,3) =-half * r1 * dr_n1
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_CMB_free_vp,     &
     &      ierr)
!
        mat_fdm_CMB_free_vp(2,1) = half * r0 * mat_fdm_CMB_free_vp(3,1)
        mat_fdm_CMB_free_vp(2,2) = half * r0 * mat_fdm_CMB_free_vp(3,2)
        mat_fdm_CMB_free_vp(2,3) = half * r0 * mat_fdm_CMB_free_vp(3,3)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix free slip CMB mat_vp ',             &
     &             nlayer_CMB, radius_1d_rj_r(nlayer_CMB)
      end if
!
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = zero
      mat_taylor_3(2,2) = one
      mat_taylor_3(2,3) = zero
!
      mat_taylor_3(3,1) = one - two*dr_n1/r0
      mat_taylor_3(3,2) = zero
      mat_taylor_3(3,3) = half * dr_n1 * dr_n1
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_CMB_free_vt,     &
     &      ierr)
!
      mat_fdm_CMB_free_vt(3,1) = two * mat_fdm_CMB_free_vt(1,1) / r0
      mat_fdm_CMB_free_vt(3,2) = two * mat_fdm_CMB_free_vt(1,2) / r0
      mat_fdm_CMB_free_vt(3,3) = two * mat_fdm_CMB_free_vt(1,3) / r0
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix free slip CMB mat_vt ',             &
     &             nlayer_CMB, radius_1d_rj_r(nlayer_CMB)
      end if
!
      end subroutine cal_2nd_nod_CMB_free_bc_fdm
!
! -----------------------------------------------------------------------
!
      subroutine set_free_cmb_fdm_mat_coefs
!
!
      coef_fdm_free_CMB_vp2(0, 1) = mat_fdm_CMB_free_vp(1,1)
      coef_fdm_free_CMB_vp2(-1,1) = mat_fdm_CMB_free_vp(1,3)
      coef_fdm_free_CMB_vp2(0, 2) = mat_fdm_CMB_free_vp(2,1)
      coef_fdm_free_CMB_vp2(-1,2) = mat_fdm_CMB_free_vp(2,3)
      coef_fdm_free_CMB_vp2(0, 3) = mat_fdm_CMB_free_vp(3,1)
      coef_fdm_free_CMB_vp2(-1,3) = mat_fdm_CMB_free_vp(3,3)
!
      coef_fdm_free_CMB_vt2(0, 1) = mat_fdm_CMB_free_vt(1,1)
      coef_fdm_free_CMB_vt2(-1,1) = mat_fdm_CMB_free_vt(1,3)
      coef_fdm_free_CMB_vt2(0, 2) = mat_fdm_CMB_free_vt(2,1)
      coef_fdm_free_CMB_vt2(-1,2) = mat_fdm_CMB_free_vt(2,3)
      coef_fdm_free_CMB_vt2(0, 3) = mat_fdm_CMB_free_vt(3,1)
      coef_fdm_free_CMB_vt2(-1,3) = mat_fdm_CMB_free_vt(3,3)
!
      end subroutine set_free_cmb_fdm_mat_coefs
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_free_CMB
!
!
      write(50,*) ' coef_fdm_free_CMB_vp2'
      write(50,*) ' mat_fdm11,  mat_fdm12'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vp2(-1:0,1)
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vp2(-1:0,2)
      write(50,*) ' mat_fdm31,  mat_fdm32'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vp2(-1:0,3)
!
      write(50,*) ' coef_fdm_free_CMB_vt2'
      write(50,*) ' mat_fdm11,  mat_fdm12'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vt2(-1:0,1)
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vt2(-1:0,2)
      write(50,*) ' mat_fdm31,  mat_fdm32'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vt2(-1:0,3)
!
      end subroutine check_coef_fdm_free_CMB
!
! -----------------------------------------------------------------------
!
      end module m_coef_fdm_free_CMB
