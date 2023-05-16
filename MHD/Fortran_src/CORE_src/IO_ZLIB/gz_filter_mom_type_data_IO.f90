!gz_filter_mom_type_data_IO.f90
!     module gz_filter_mom_type_data_IO
!
!     Written by H. Matsui on Feb., 2012
!
!!      subroutine gz_write_filter_elen_data(FPz_f, FEM_elens, zbuf)
!!      subroutine gz_write_filter_moms_data                            &
!!     &         (FPz_f, FEM_elens, FEM_moms, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!!
!!      subroutine gz_read_filter_moment_num                            &
!!     &         (FPz_f, FEM_elens, FEM_moms, zbuf)
!!      subroutine gz_read_filter_elen_data                             &
!!     &         (FPz_f, numnod, numele, FEM_elens, zbuf, ierr)
!!      subroutine gz_read_filter_moms_data(FPz_f, numnod, numele,      &
!!     &          FEM_elens, FEM_moms, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer (kind=kint), intent(in) :: numnod, numele
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!
      module gz_filter_mom_type_data_IO
!
      use m_precision
!
      use t_filter_elength
      use t_buffer_4_gzip
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_filter_elen_data(FPz_f, FEM_elens, zbuf)
!
      use gz_filter_moments_IO
      use gz_filter_moms_elen_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call write_filter_elen_head_gz(FPz_f, FEM_elens%nnod_filter_mom,  &
     &    FEM_elens%nele_filter_mom, FEM_elens%filter_conf%nf_type,     &
     &    zbuf)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_ref_filter_param_gz                                  &
     &     (FPz_f, FEM_elens%filter_conf, zbuf)
        call write_elens_ele_gz(FPz_f, FEM_elens%nele_filter_mom,       &
     &      FEM_elens%elen_ele%moms, FEM_elens%elen_ele%diff,           &
     &      FEM_elens%elen_ele%diff2, zbuf)
      end if
!
      call dealloc_filter_mom_type(FEM_elens)
!
      end subroutine gz_write_filter_elen_data
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_filter_moms_data                              &
     &         (FPz_f, FEM_elens, FEM_moms, zbuf)
!
      use t_filter_moments
      use gz_filter_moments_IO
      use gz_filter_moms_elen_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer (kind = kint) :: ifil
!
!
      call write_filter_moms_head_gz(FPz_f, FEM_moms%nnod_fmom,         &
     &    FEM_moms%nele_fmom, FEM_moms%num_filter_moms,                 &
     &    FEM_elens%filter_conf%nf_type, zbuf)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_ref_filter_param_gz                                  &
     &     (FPz_f, FEM_elens%filter_conf, zbuf)
        do ifil = 1, FEM_moms%num_filter_moms
          call write_filter_moms_ele_gz(FPz_f, FEM_moms%nele_fmom,      &
     &        FEM_moms%mom_ele(ifil)%moms, FEM_moms%mom_ele(ifil)%diff, &
     &        FEM_moms%mom_ele(ifil)%diff2, zbuf)
        end do
      end if
!
      call dealloc_filter_moms_ele_type(FEM_moms)
!
      end subroutine gz_write_filter_moms_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_read_filter_moment_num                              &
     &         (FPz_f, FEM_elens, FEM_moms, zbuf)
!
      use t_filter_moments
      use gz_filter_moments_IO
!
      character, pointer, intent(in) :: FPz_f
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call read_filter_moms_head_gz(FPz_f, FEM_elens%nnod_filter_mom,   &
     &    FEM_elens%nele_filter_mom, FEM_moms%num_filter_moms,          &
     &    FEM_elens%filter_conf%nf_type, zbuf)
!
      end subroutine gz_read_filter_moment_num
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_read_filter_elen_data                               &
     &         (FPz_f, numnod, numele, FEM_elens, zbuf, ierr)
!
      use gz_filter_moments_IO
      use gz_filter_moms_elen_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer (kind=kint), intent(in) :: numnod, numele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer (kind=kint), intent(inout) :: ierr
!
!
      call read_filter_elen_head_gz(FPz_f, FEM_elens%nnod_filter_mom,   &
     &    FEM_elens%nele_filter_mom, FEM_elens%filter_conf%nf_type,     &
     &    zbuf)
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        ierr = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_elen_ele_type(FEM_elens%nele_filter_mom,               &
     &    FEM_elens%elen_ele)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call read_ref_filter_param_gz                                   &
     &     (FPz_f, FEM_elens%filter_conf, zbuf)
        call read_elens_ele_gz(FPz_f, FEM_elens%nele_filter_mom,        &
     &      FEM_elens%elen_ele%moms, FEM_elens%elen_ele%diff,           &
     &      FEM_elens%elen_ele%diff2, zbuf)
      end if
!
      end subroutine gz_read_filter_elen_data
!
! ----------------------------------------------------------------------
!
      subroutine gz_read_filter_moms_data(FPz_f, numnod, numele,        &
     &          FEM_elens, FEM_moms, zbuf, ierr)
!
      use t_filter_moments
      use gz_filter_moms_elen_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer (kind=kint), intent(in) :: numnod, numele
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer (kind=kint), intent(inout) :: ierr
!
      integer (kind=kint) :: ifil
!
!
      call gz_read_filter_moment_num(FPz_f, FEM_elens, FEM_moms, zbuf)
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        ierr = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        ierr = 501
      else
        ierr = 0
      end if
!
      FEM_moms%nnod_fmom = FEM_elens%nnod_filter_mom
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_filter_moms_ele_type(FEM_elens%nele_filter_mom,        &
     &    FEM_moms)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
!
        call read_ref_filter_param_gz                                   &
     &     (FPz_f, FEM_elens%filter_conf, zbuf)
        do ifil = 1, FEM_moms%num_filter_moms
          call read_filter_moms_ele_gz(FPz_f, FEM_moms%nele_fmom,       &
     &        FEM_moms%mom_ele(ifil)%moms, FEM_moms%mom_ele(ifil)%diff, &
     &        FEM_moms%mom_ele(ifil)%diff2, zbuf)
        end do
!
      end if
!
      end subroutine gz_read_filter_moms_data
!
! ----------------------------------------------------------------------
!
      end module gz_filter_mom_type_data_IO
