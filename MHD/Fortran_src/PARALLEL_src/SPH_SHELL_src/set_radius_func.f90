!
!      module set_radius_func
!
!      Programmed by H. Matsui on June., 1994
!      modified by H. Matsui on Apr., 2009
!
!      subroutine set_radius_rot_reft_dat_4_sph(r_in, r_out,            &
!     &          temp_ICB, temp_CMB, rotate)
!      subroutine cal_fdm_matrices
!
      module set_radius_func
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
!
      implicit none
!
      private :: cal_2nd_ele_r_fdm_coefs
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_radius_rot_reft_dat_4_sph(r_in, r_out,             &
     &          temp_ICB, temp_CMB, rotate)
!
      use m_sph_spectr_data
      use m_sph_phys_address
      use set_radius_func_noequi
      use set_radius_func_cheby
!
      use set_radius_4_sph_dynamo
      use set_reference_temp_sph
      use set_poloidal_rotation
!
      real(kind = kreal), intent(in) :: r_in, r_out
      real(kind = kreal), intent(in) :: temp_ICB, temp_CMB
      real(kind = kreal), intent(in) :: rotate(3)
!
      
!
!* --------  radius  --------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_radius_dat_4_sph_dynamo'
      call set_radius_dat_4_sph_dynamo
!
      if (iflag_debug.eq.1) write(*,*) 'Set grid spacing',              &
     &                                 iflag_radial_grid
      if (iflag_radial_grid .eq. igrid_Chebyshev) then
        call set_dr_for_cheby
      else if (iflag_radial_grid .eq. igrid_non_euqidist) then
        call set_dr_for_nonequi
      else
        call set_dr_for_equidist
      end if
!
      if(iflag_debug .gt. 0) call check_radial_fung_rj
!
!*  ----------   reference of temperature --------
!*
      if (ipol%i_ref_t .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'set_reftemp_4_sph'
        call set_reftemp_4_sph(r_in, r_out, temp_ICB, temp_CMB)
      end if
!*
!*  ----------  rotation of earth  ---------------
!*
      if (iflag_debug.eq.1) write(*,*) 'set_rot_earth_4_sph'
      call set_rot_earth_4_sph(rotate)
!
      end subroutine set_radius_rot_reft_dat_4_sph
!
!  -------------------------------------------------------------------
!
      subroutine cal_fdm_matrices
!
      use m_fdm_matrix
!
      use set_radius_func_noequi
      use set_radius_func_cheby
!
!
      call allocate_fdm_matrices(nidx_rj(1))
!
      if (iflag_radial_grid .eq. igrid_Chebyshev) then
        call nod_r_2nd_fdm_coefs_cheby
      else if (iflag_radial_grid .eq. igrid_non_euqidist) then
        call nod_r_2nd_fdm_coefs_nonequi
      else
        call nod_r_2nd_fdm_coefs_equi
      end if
!
      call cal_2nd_ele_r_fdm_coefs
!
!     if(iflag_debug .gt. 0) then
!       call check_fdm_2_mat(nidx_rj(1), radius_1d_rj_r(1))
!       call check_fdm_2e_mat(nidx_rj(1), radius_1d_rj_r(1))
!      end if
!
      end subroutine cal_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_ele_r_fdm_coefs
!
      use m_fdm_matrix
      use cal_inverse_small_matrix
!
      integer(kind = kint) :: ierr
      integer(kind = kint) :: kr
!
      real(kind = kreal) :: dr_p1, dr_n1
      real(kind = kreal) :: mat_taylor_2(2,2)
!
!
      do kr = 1, nidx_rj(1)
!
        dr_p1 = dr_1d_rj(kr,0) * half
        if (kr.eq.1) then
          if(nlayer_ICB.gt.1) then
            dr_n1 = radius_1d_rj_r(1) * half
          else
            dr_n1 = dr_1d_rj(1,0) * half
          end if
        else
          dr_n1 = dr_1d_rj(kr,1) * half
        end if
!
        mat_taylor_2(1,1) = one
        mat_taylor_2(1,2) = dr_p1
!
        mat_taylor_2(2,1) = one
        mat_taylor_2(2,2) =-dr_n1
!
        call cal_inverse_22_matrix(mat_taylor_2, mat_fdm_2e(1,1,kr),    &
     &      ierr)
      end do
!
      end subroutine cal_2nd_ele_r_fdm_coefs
!
! -----------------------------------------------------------------------
!
      end module set_radius_func
