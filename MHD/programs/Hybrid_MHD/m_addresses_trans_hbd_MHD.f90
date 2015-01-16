!>@file   m_addresses_trans_hbd_MHD.f90
!!@brief  module m_addresses_trans_hbd_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in hybrid dynamo
!!
!!@verbatim
!!      subroutine allocate_hbd_trans_rtp
!!      subroutine deallocate_hbd_trans_rtp
!!
!!      subroutine set_addresses_trans_hbd_MHD
!!      subroutine check_add_trans_hbd_MHD
!!@endverbatim
!
      module m_addresses_trans_hbd_MHD
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!>      number of components for backward spherical harmonics transform
      integer(kind = kint) :: ncomp_rj_2_xyz = 0
!>      number of components
!!      for backward vector spherical harmonics transform
      integer(kind = kint) :: nvector_rj_2_xyz = 0
!>      number of scalars for backward spherical harmonics transform
      integer(kind = kint) :: nscalar_rj_2_xyz = 0
!>      number of tensors for backward spherical harmonics transform
      integer(kind = kint) :: ntensor_rj_2_xyz = 0
!
!>      number of components for forward spherical harmonics transform
      integer(kind = kint) :: ncomp_xyz_2_rj =   0
!>      number of vectors for forward spherical harmonics transform
      integer(kind = kint) :: nvector_xyz_2_rj = 0
!>      number of scalars for forward spherical harmonics transform
      integer(kind = kint) :: nscalar_xyz_2_rj = 0
!>      number of tensors for forward spherical harmonics transform
      integer(kind = kint) :: ntensor_xyz_2_rj = 0
!
!
!>      addresses of fields for backward transform
      type(phys_address), save :: b_hbd_trns
!
!>      addresses of forces for forward transform
      type(phys_address), save :: f_hbd_trns
!
!>      field data to evaluate nonliear terms in grid space
      real(kind = kreal), allocatable :: fld_hbd_rtp(:,:)
!>      Nonoliear terms data in grid space
      real(kind = kreal), allocatable :: frc_hbd_rtp(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_hbd_trans_rtp
!
      use m_spheric_parameter
!
!
      allocate(fld_hbd_rtp(nnod_rtp,ncomp_rj_2_xyz))
      allocate(frc_hbd_rtp(nnod_rtp,ncomp_xyz_2_rj))
      if(ncomp_rj_2_xyz .gt. 0) fld_hbd_rtp = 0.0d0
      if(ncomp_xyz_2_rj .gt. 0) frc_hbd_rtp = 0.0d0
!
      end subroutine allocate_hbd_trans_rtp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_hbd_trans_rtp
!
      deallocate(fld_hbd_rtp, frc_hbd_rtp)
!
      end subroutine deallocate_hbd_trans_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_hbd_MHD
!
      use m_control_parameter
      use m_work_4_sph_trans
!
!
      nvector_rj_2_xyz = 0
!   magnetic field flag
      if(       iflag_t_evo_4_magne .gt. id_no_evolution                &
     &     .or. iflag_4_lorentz .gt.     id_turn_OFF) then
        nvector_rj_2_xyz = nvector_rj_2_xyz + 1
        b_hbd_trns%i_magne = 3*nvector_rj_2_xyz - 2
      end if
!   current density flag
      if(iflag_4_lorentz .gt. id_turn_OFF) then
        nvector_rj_2_xyz = nvector_rj_2_xyz + 1
        b_hbd_trns%i_current = 3*nvector_rj_2_xyz - 2
      end if
!   magnetic diffusion flag
      if(iflag_t_evo_4_magne .gt. id_turn_OFF) then
        nvector_rj_2_xyz = nvector_rj_2_xyz + 1
        b_hbd_trns%i_b_diffuse = 3*nvector_rj_2_xyz - 2
      end if
!    magnetic induction flag
      if(iflag_t_evo_4_magne .gt. id_turn_OFF) then
        nvector_rj_2_xyz = nvector_rj_2_xyz + 1
        b_hbd_trns%i_induction = 3*nvector_rj_2_xyz - 2
      end if
!    SGS magnetic induction flag
      if(iflag_SGS_induction .gt. id_SGS_none) then
        nvector_rj_2_xyz = nvector_rj_2_xyz + 1
        b_hbd_trns%i_SGS_induction = 3*nvector_rj_2_xyz - 2
      end if
      ncomp_rj_2_xyz = 3*nvector_rj_2_xyz
!
!
      nscalar_rj_2_xyz = 0
      ncomp_rj_2_xyz = ncomp_rj_2_xyz + nscalar_rj_2_xyz
!
!
      nvector_xyz_2_rj = 0
!   induction flag
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        nvector_xyz_2_rj = nvector_xyz_2_rj + 1
        f_hbd_trns%i_vp_induct =  3*nvector_xyz_2_rj - 2
      end if
!    SGS magnetic induction flag
      if(iflag_SGS_induction .gt. id_SGS_none) then
        nvector_xyz_2_rj = nvector_xyz_2_rj + 1
        f_hbd_trns%i_SGS_vp_induct =  3*nvector_xyz_2_rj - 2
      end if
      ncomp_xyz_2_rj = 3*nvector_xyz_2_rj
!
      nscalar_xyz_2_rj = 0
      ncomp_xyz_2_rj = ncomp_xyz_2_rj + nscalar_xyz_2_rj
!
!      ncomp_sph_trans = max(ncomp_rj_2_xyz, ncomp_xyz_2_rj)
!      nvector_sph_trans                                                 &
!     &      = max(nvector_rj_2_xyz, nvector_xyz_2_rj)
!      nscalar_sph_trans                                                 &
!     &      = max((nscalar_rj_2_xyz+6*ntensor_rj_2_xyz),        &
!     &            (nscalar_xyz_2_rj+6*ntensor_xyz_2_rj))
!
      end subroutine set_addresses_trans_hbd_MHD
!
!-----------------------------------------------------------------------
!
      subroutine check_add_trans_hbd_MHD
!
      use m_sph_phys_address
      use m_work_4_sph_trans
!
!
      write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
      write(*,*) 'ncomp_rj_2_xyz  ', ncomp_rj_2_xyz
      write(*,*) 'ncomp_xyz_2_rj  ', ncomp_xyz_2_rj
!
      write(*,*) 'nvector_rj_2_xyz  ', nvector_rj_2_xyz
      if(b_hbd_trns%i_magne .gt. 0) write(*,*)                          &
     &        'b_hbd_trns%i_magne ', b_hbd_trns%i_magne,                &
     &        ipol%i_magne, itor%i_magne, idpdr%i_magne
      if(b_hbd_trns%i_current .gt. 0) write(*,*)                        &
     &        'b_hbd_trns%i_current ', b_hbd_trns%i_current,            &
     &        ipol%i_current, itor%i_current, idpdr%i_current
      if(b_hbd_trns%i_b_diffuse .gt. 0) write(*,*)                      &
     &        'b_hbd_trns%i_b_diffuse ', b_hbd_trns%i_b_diffuse,        &
     &        ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
      if(b_hbd_trns%i_induction .gt. 0) write(*,*)                      &
     &        'b_hbd_trns%i_induction ', b_hbd_trns%i_induction,        &
     &        ipol%i_induction, itor%i_induction, idpdr%i_induction
      if(b_hbd_trns%i_SGS_induction .gt. 0) write(*,*)                  &
     &       'b_hbd_trns%i_SGS_induction ', b_hbd_trns%i_SGS_induction, &
     &       ipol%i_SGS_induction, itor%i_SGS_induction,                &
     &       idpdr%i_SGS_induction
      write(*,*)
!
      write(*,*) 'nscalar_rj_2_xyz  ', nscalar_rj_2_xyz
      write(*,*)
!
      write(*,*) 'nvector_xyz_2_rj  ', nvector_xyz_2_rj
      if(f_hbd_trns%i_vp_induct .gt. 0) write(*,*)                      &
     &       'f_hbd_trns%i_vp_induct ', f_hbd_trns%i_vp_induct,         &
     &        ipol%i_vp_induct, itor%i_vp_induct, idpdr%i_vp_induct
      if(f_hbd_trns%i_SGS_vp_induct .gt. 0) write(*,*)                  &
     &       'f_hbd_trns%i_SGS_vp_induct', f_hbd_trns%i_SGS_vp_induct,  &
     &        ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,               &
     &        idpdr%i_SGS_vp_induct
!
      write(*,*) 'nscalar_xyz_2_rj  ', nscalar_xyz_2_rj
      write(*,*)
!
      end subroutine check_add_trans_hbd_MHD
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_hbd_MHD
