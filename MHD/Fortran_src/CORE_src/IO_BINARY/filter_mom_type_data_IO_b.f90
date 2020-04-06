!filter_mom_type_data_IO_b.f90
!     module filter_mom_type_data_IO_b
!
!     Written by H. Matsui on Feb., 2012
!
!!      subroutine write_filter_elen_data_b(FEM_elens, bbuf)
!!      subroutine write_filter_moms_data_b(FEM_elens, FEM_moms, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!!
!!      subroutine read_filter_moment_num_b(bbuf, FEM_elens, FEM_moms)
!!      subroutine read_filter_elen_data_b(numnod, numele,              &
!!     &          bbuf, FEM_elens)
!!      subroutine read_filter_moms_data_b                              &
!!     &         (numnod, numele, bbuf, FEM_elens, FEM_moms)
!!        integer (kind=kint), intent(in) :: numnod, numele
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      module filter_mom_type_data_IO_b
!
      use m_precision
!
      use t_filter_elength
      use t_binary_IO_buffer
      use transfer_to_long_integers
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_elen_data_b(FEM_elens, bbuf)
!
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_one_integer_b(FEM_elens%nnod_filter_mom, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(FEM_elens%nele_filter_mom, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(FEM_elens%filter_conf%nf_type, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_b                              &
     &     (FEM_elens%filter_conf, bbuf)
        if(bbuf%ierr_bin .ne. 0) return
        call write_elen_ele_type_b                                      &
     &     (cast_long(FEM_elens%nele_filter_mom), FEM_elens%elen_ele,   &
     &      bbuf)
        if(bbuf%ierr_bin .ne. 0) return
      end if
!
      call dealloc_filter_mom_type(FEM_elens)
!
      end subroutine write_filter_elen_data_b
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_moms_data_b(FEM_elens, FEM_moms, bbuf)
!
      use t_filter_moments
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer (kind = kint) :: ifil
!
!
      call write_one_integer_b(FEM_moms%nnod_fmom, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(FEM_moms%nele_fmom, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(FEM_moms%num_filter_moms, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(FEM_elens%filter_conf%nf_type, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call write_base_filter_info_type_b                              &
     &     (FEM_elens%filter_conf, bbuf)
        if(bbuf%ierr_bin .ne. 0) return
        do ifil = 1, FEM_moms%num_filter_moms
          call write_filter_moms_ele_type_b                             &
     &       (cast_long(FEM_moms%nele_fmom), FEM_moms%mom_ele(ifil),    &
     &        bbuf)
          if(bbuf%ierr_bin .ne. 0) return
        end do
      end if
!
      call dealloc_filter_moms_ele_type(FEM_moms)
!
      end subroutine write_filter_moms_data_b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_moment_num_b(bbuf, FEM_elens, FEM_moms)
!
      use t_filter_moments
      use filter_moments_IO_b
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      call read_one_integer_b(bbuf, FEM_elens%nnod_filter_mom)
      if(bbuf%ierr_bin .gt. 0) return
      call read_one_integer_b(bbuf, FEM_elens%nele_filter_mom)
      if(bbuf%ierr_bin .gt. 0) return
      call read_one_integer_b(bbuf, FEM_moms%num_filter_moms)
      if(bbuf%ierr_bin .gt. 0) return
      call read_one_integer_b(bbuf, FEM_elens%filter_conf%nf_type)
!
      end subroutine read_filter_moment_num_b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_elen_data_b(numnod, numele,                &
     &          bbuf, FEM_elens)
!
      use filter_moments_IO_b
      use filter_mom_type_on_ele_IO_b
!
!
      integer (kind=kint), intent(in) :: numnod, numele
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      call read_one_integer_b(bbuf, FEM_elens%nnod_filter_mom)
      if(bbuf%ierr_bin .gt. 0) return
      call read_one_integer_b(bbuf, FEM_elens%nele_filter_mom)
      if(bbuf%ierr_bin .gt. 0) return
      call read_one_integer_b(bbuf, FEM_elens%filter_conf%nf_type)
      if(bbuf%ierr_bin .gt. 0) return
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        bbuf%ierr_bin = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        bbuf%ierr_bin = 501
      else
        bbuf%ierr_bin = 0
      end if
      if(bbuf%ierr_bin .ne. 0) return
!
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_elen_ele_type(FEM_elens%nele_filter_mom,               &
     &    FEM_elens%elen_ele)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
        call read_base_filter_info_type_b(bbuf, FEM_elens%filter_conf)
        if(bbuf%ierr_bin .ne. 0) return
        call read_elen_ele_type_b(bbuf,                                 &
     &      cast_long(FEM_elens%nele_filter_mom), FEM_elens%elen_ele)
        if(bbuf%ierr_bin .ne. 0) return
      end if
!
      end subroutine read_filter_elen_data_b
!
! ----------------------------------------------------------------------
!
      subroutine read_filter_moms_data_b                                &
     &         (numnod, numele, bbuf, FEM_elens, FEM_moms)
!
      use t_filter_moments
      use filter_mom_type_on_ele_IO_b
!
      integer (kind=kint), intent(in) :: numnod, numele
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
      integer (kind=kint) :: ifil
!
!
      call read_filter_moment_num_b(bbuf, FEM_elens, FEM_moms)
      if(bbuf%ierr_bin .gt. 0) return
!
      if (FEM_elens%nnod_filter_mom.ne.numnod) then
        bbuf%ierr_bin = 500
      else if (FEM_elens%nele_filter_mom.ne.numele) then
        bbuf%ierr_bin = 501
      else
        bbuf%ierr_bin = 0
      end if
      if(bbuf%ierr_bin .ne. 0) return
!
      FEM_moms%nnod_fmom = FEM_elens%nnod_filter_mom
      call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
      call alloc_filter_moms_ele_type(FEM_elens%nele_filter_mom,        &
     &    FEM_moms)
!
      if (FEM_elens%filter_conf%nf_type .gt. 0) then
!
        call read_base_filter_info_type_b(bbuf, FEM_elens%filter_conf)
        if(bbuf%ierr_bin .ne. 0) return
!
        do ifil = 1, FEM_moms%num_filter_moms
          call read_filter_moms_ele_type_b(bbuf,                        &
     &        cast_long(FEM_moms%nele_fmom), FEM_moms%mom_ele(ifil))
          if(bbuf%ierr_bin .ne. 0) return
        end do
!
      end if
!
      end subroutine read_filter_moms_data_b
!
! ----------------------------------------------------------------------
!
      end module filter_mom_type_data_IO_b
