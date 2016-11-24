!>@file   check_gaunt_coriolis_rlm.f90
!!@brief  module check_gaunt_coriolis_rlm
!!
!!@author H. Matsui
!!@date Programmed in 1994
!!@n Modified in 2010
!
!>@brief  Coefficients for Coriolis term on f(r,l,m)
!!
!!@verbatim
!!      subroutine s_check_gaunt_coriolis_rlm(iflag_sph_coriolis_file,  &
!!     &          jmax_tri_sph, sph_params, sph_rlm)
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!      subroutine check_gaunt_coriolis_rlm(iflag_sph_coriolis_file)
!!@endverbatim
      module check_gaunt_coriolis_rlm
!*
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: check_gaunt_integrals_rlm, check_interact_coriolis_rlm
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_check_gaunt_coriolis_rlm(iflag_sph_coriolis_file,    &
     &          jmax_tri_sph, sph_params, sph_rlm)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: iflag_sph_coriolis_file
      integer(kind = kint), intent(in) :: jmax_tri_sph
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      call check_gaunt_integrals_rlm(iflag_sph_coriolis_file,           &
     &   jmax_tri_sph, sph_params%l_truncation,                         &
     &    sph_rlm%nidx_rlm(2), sph_rlm%idx_gl_1d_rlm_j)
      call check_interact_coriolis_rlm                                  &
     &   (sph_rlm%nidx_rlm(2), sph_rlm%idx_gl_1d_rlm_j)
!
      end subroutine s_check_gaunt_coriolis_rlm
!
!-----------------------------------------------------------------------
!
      subroutine check_gaunt_integrals_rlm(iflag_sph_coriolis_file,     &
     &          jmax_tri_sph, l_truncation, jmax_rlm, idx_gl_1d_rlm_j)
!
      use m_gaunt_coriolis_rlm
      use m_integrals_4_sph_coriolis
      use m_int_4_sph_coriolis_IO
      use sph_coriolis_IO_select
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: iflag_sph_coriolis_file
      integer(kind = kint), intent(in) :: jmax_tri_sph
      integer(kind = kint), intent(in) :: l_truncation, jmax_rlm
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(jmax_rlm,3)
!
      integer(kind = kint) :: l3, m3, j3, j_rlm
      integer(kind = kint) :: jgi_cor_ref1, jgi_cor_ref2, jei_cor_ref
      integer(kind = kint) :: lgi_cor_ref1, lgi_cor_ref2, lei_cor_ref
      integer(kind = kint) :: mgi_cor_ref1, mgi_cor_ref2, mei_cor_ref
      real(kind = kreal) :: err
!
!
      if(iflag_sph_coriolis_file .eq. 0) then
        call sel_read_int_4_sph_coriolis
        if(iflag_debug.eq.1) write(*,*) 'copy_int_sph_coriolis_from_IO'
        call copy_int_sph_coriolis_from_IO(l_truncation, jmax_tri_sph)
      end if
!
      do j_rlm = 1, jmax_rlm
        j3 = idx_gl_1d_rlm_j(j_rlm,1)
        l3 = idx_gl_1d_rlm_j(j_rlm,2)
        m3 = idx_gl_1d_rlm_j(j_rlm,3)
!
        call get_dgree_order_by_full_j                                  &
     &     (jgl_kcor(j3,1,2), lgi_cor_ref1, mgi_cor_ref1)
        call get_dgree_order_by_full_j                                  &
     &     (jgl_kcor(j3,2,2), lgi_cor_ref2, mgi_cor_ref2)
        call get_dgree_order_by_full_j                                  &
     &     (jgl_lcor(j3,1,2), lei_cor_ref, mei_cor_ref)
        jgi_cor_ref1 = find_local_sph_rlm_address(jmax_rlm,             &
     &                idx_gl_1d_rlm_j, lgi_cor_ref1, mgi_cor_ref1)
        jgi_cor_ref2 = find_local_sph_rlm_address(jmax_rlm,             &
     &                idx_gl_1d_rlm_j, lgi_cor_ref2, mgi_cor_ref2)
        jei_cor_ref = find_local_sph_rlm_address(jmax_rlm,              &
     &                idx_gl_1d_rlm_j, lei_cor_ref, mei_cor_ref)
!
        err = abs(gi_cor_rlm(j_rlm,1)-gk_cor(j3,1,2)) / gk_cor(j3,1,2)
        if(jgi_cor_rlm(j_rlm,1).ne.jgi_cor_ref1 .or. err .gt. 1.0E-11)  &
     &      write(*,*) 'wrong jgi_cor_rlm(j_rlm,1)', j3, l3, m3,        &
     &                 jgi_cor_rlm(j_rlm,1), jgi_cor_ref1,              &
     &                 gi_cor_rlm(j_rlm,1), gk_cor(j3,1,2)
!
        err = abs(gi_cor_rlm(j_rlm,2)-gk_cor(j3,2,2)) / gk_cor(j3,2,2)
        if(jgi_cor_rlm(j_rlm,2).ne.jgi_cor_ref2 .or. err .gt. 1.0E-11)  &
     &      write(*,*) 'wrong jgi_cor_rlm(j_rlm,2)', j3, l3, m3,        &
     &                 jgi_cor_rlm(j_rlm,2), jgi_cor_ref2,              &
     &                 gi_cor_rlm(j_rlm,2), gk_cor(j3,2,2)
!
        err = abs(ei_cor_rlm(j_rlm,1)-el_cor(j3,1,2)) / el_cor(j3,1,2)
        if(jei_cor_rlm(j_rlm,1) .ne. jei_cor_ref .or. err .gt. 1.0E-11) &
     &      write(*,*) 'wrong jei_cor_rlm(j_rlm,1)', j3, l3, m3,        &
     &                 jei_cor_rlm(j_rlm,1), jei_cor_ref,               &
     &                 ei_cor_rlm(j_rlm,1), el_cor(j3,1,2)
      end do
!
      end subroutine check_gaunt_integrals_rlm
!
!-----------------------------------------------------------------------
!
      subroutine check_interact_coriolis_rlm(jmax_rlm, idx_gl_1d_rlm_j)
!
      use m_gaunt_coriolis_rlm
      use m_coriolis_coefs_tri_sph
!
      integer(kind = kint), intent(in) :: jmax_rlm
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(jmax_rlm,3)
      integer(kind = kint) :: j3, j_gl
      real(kind = kreal) :: err
!
!
      do j3 = 1, jmax_rlm
        j_gl = idx_gl_1d_rlm_j(j3,1)
        err =  abs(sw_rlm(1,1,j3) - sw_rj(1,1,j_gl))                    &
     &       + abs(sw_rlm(2,1,j3) - sw_rj(2,1,j_gl))                    &
     &       + abs(sw_rlm(1,2,j3) - sw_rj(1,2,j_gl))                    &
     &       + abs(sw_rlm(2,2,j3) - sw_rj(2,2,j_gl))                    &
     &       + abs(sw_rlm(1,3,j3) - sw_rj(1,3,j_gl))
        if(err .gt. 1.0d-11) write(*,*) 'Error in sw_rlm', j3, j_gl,    &
     &         sw_rlm(1,1,j3), sw_rj(1,1,j_gl),                         &
     &         sw_rlm(2,1,j3), sw_rj(2,1,j_gl),                         &
     &         sw_rlm(1,2,j3), sw_rj(1,2,j_gl),                         &
     &         sw_rlm(2,2,j3), sw_rj(2,2,j_gl),                         &
     &         sw_rlm(1,3,j3), sw_rj(1,3,j_gl)
!
        err =  abs(tw_rlm(1,3,j3) - tw_rj(1,3,j_gl))                    &
     &       + abs(tw_rlm(2,3,j3) - tw_rj(2,3,j_gl))                    &
     &       + abs(tw_rlm(1,4,j3) - tw_rj(1,4,j_gl))                    &
     &       + abs(tw_rlm(2,4,j3) - tw_rj(2,4,j_gl))                    &
     &       + abs(tw_rlm(1,1,j3) - tw_rj(1,1,j_gl))                    &
     &       + abs(tw_rlm(1,2,j3) - tw_rj(1,2,j_gl))
        if(err .gt. 1.0d-11) write(*,*) 'Error in tw_rlm', j3, j_gl,    &
     &         tw_rlm(1,3,j3), tw_rj(1,3,j_gl),                         &
     &         tw_rlm(2,3,j3), tw_rj(2,3,j_gl),                         &
     &         tw_rlm(1,4,j3), tw_rj(1,4,j_gl),                         &
     &         tw_rlm(2,4,j3), tw_rj(2,4,j_gl),                         &
     &         tw_rlm(1,1,j3), tw_rj(1,1,j_gl),                         &
     &         tw_rlm(1,2,j3), tw_rj(1,2,j_gl)
!
        err =  abs(sd_rlm(1,1,j3) - sd_rj(1,1,j_gl))                    &
     &       + abs(sd_rlm(2,1,j3) - sd_rj(2,1,j_gl))                    &
     &       + abs(sd_rlm(1,2,j3) - sd_rj(1,2,j_gl))                    &
     &       + abs(sd_rlm(2,2,j3) - sd_rj(2,2,j_gl))
        if(err .gt. 1.0d-11) write(*,*) 'Error in sd_rlm', j3, j_gl,    &
     &         sd_rlm(1,1,j3), sd_rj(1,1,j_gl),                         &
     &         sd_rlm(2,1,j3), sd_rj(2,1,j_gl),                         &
     &         sd_rlm(1,2,j3), sd_rj(1,2,j_gl),                         &
     &         sd_rlm(2,2,j3), sd_rj(2,2,j_gl)
!
        err =  abs(td_rlm(1,j3) - td_rj(1,j_gl))
        if(err .gt. 1.0d-11) write(*,*) 'Error in td_rlm', j3, j_gl,    &
     &        td_rlm(1,j3), td_rj(1,j_gl)
!
        err =  abs(tr_rlm(1,j3) - tr_rj(1,j_gl))                        &
     &       + abs(tr_rlm(2,j3) - tr_rj(2,j_gl))
        if(err .gt. 1.0d-11) write(*,*) 'Error in tr_rlm', j3, j_gl,    &
     &        tr_rlm(1,j3), tr_rj(1,j_gl), tr_rlm(2,j3), tr_rj(2,j_gl)
!
        err =  abs(sr_rlm(1,j3) - sr_rj(1,j_gl))
        if(err .gt. 1.0d-11) write(*,*) 'Error in sr_rlm', j3, j_gl,    &
     &        sr_rlm(1,j3), sr_rj(1,j_gl)
      end do
!*
      end subroutine check_interact_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      end module check_gaunt_coriolis_rlm
