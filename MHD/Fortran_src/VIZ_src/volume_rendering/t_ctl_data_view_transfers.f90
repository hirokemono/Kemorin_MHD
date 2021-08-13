!>@file   t_ctl_data_view_transfers.f90
!!@brief  module t_ctl_data_view_transfers
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!>@brief Control inputs for multiple PVR view parameter
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine dealloc_multi_modeview_ctl(mul_qmats_c)
!!      subroutine read_mul_view_transfer_ctl                           &
!!     &         (id_control, hd_block, mul_qmats_c, c_buf)
!!        type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine read_mul_view_trans_block_ctl                        &
!!     &         (id_control, hd_block, qmat_file_ctl, c_buf)
!!        type(modeview_and_fname_ctl), intent(inout) :: qmat_file_ctl
!!
!!      subroutine bcast_mul_view_trans_ctl(mul_qmats_c)
!!        type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
!!
!!      subroutine append_mul_view_trans_ctl(mul_qmats_c)
!!        type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
!!      subroutine dup_mul_view_trans_ctl(org_mul_qmats_c,              &
!!     &                                  new_mul_qmats_c)
!!        type(multi_modeview_ctl), intent(in) :: org_mul_qmats_c
!!        type(multi_modeview_ctl), intent(inout) :: new_mul_qmats_c
!!      subroutine copy_mul_view_trans_ctl                              &
!!     &         (num_qmat, org_qmat_ctl, new_qmat_ctl)
!!        type(modeview_and_fname_ctl), intent(in)                      &
!!     &                       :: org_qmat_ctl(num_qmat)
!!        type(modeview_and_fname_ctl), intent(inout)                   &
!!     &                       :: new_qmat_ctl(num_qmat)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array view_transform_ctl
!!      file  view_transform_ctl  control_view
!!
!!      begin view_transform_ctl
!!        ..
!!      end
!!    end array view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_view_transfers
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_ctl_data_4_view_transfer
      use skip_comment_f
!
      implicit  none
!
!
      type modeview_and_fname_ctl
        character(len = kchara) :: fname_qmat_ctl
!         Lists of view parameters
        type(modeview_ctl) :: q_mat
      end type modeview_and_fname_ctl
!
!
      type multi_modeview_ctl
        integer(kind = kint) :: num_mul_qmats_c = 0
        type(modeview_and_fname_ctl), allocatable :: qmat_file_ctl(:)
      end type multi_modeview_ctl
!
      private :: read_mul_view_trans_block_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_multi_modeview_ctl(mul_qmats_c)
!
      type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
!
!
     if(allocated(mul_qmats_c%qmat_file_ctl)) then
        call dealloc_mul_view_trans_ctl                                 &
     &     (mul_qmats_c%num_mul_qmats_c, mul_qmats_c%qmat_file_ctl)
        deallocate(mul_qmats_c%qmat_file_ctl)
      end if
!
      mul_qmats_c%num_mul_qmats_c = 0
!
      end subroutine dealloc_multi_modeview_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_multi_modeview_ctl(mul_qmats_c)
!
      type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
!
!
      allocate(mul_qmats_c%qmat_file_ctl(mul_qmats_c%num_mul_qmats_c))
!
      end subroutine alloc_multi_modeview_ctl
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_mul_view_transfer_ctl                             &
     &         (id_control, hd_block, mul_qmats_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(allocated(mul_qmats_c%qmat_file_ctl)) return
      mul_qmats_c%num_mul_qmats_c = 0
      call alloc_multi_modeview_ctl(mul_qmats_c)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_mul_view_trans_ctl(mul_qmats_c)
!
          write(*,'(2a,i4)', ADVANCE='NO') trim(hd_block),              &
     &                           ' No. ', mul_qmats_c%num_mul_qmats_c
          call read_mul_view_trans_block_ctl(id_control, hd_block,      &
     &        mul_qmats_c%qmat_file_ctl(mul_qmats_c%num_mul_qmats_c),   &
     &        c_buf)
        end if
      end do
!
      end subroutine read_mul_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_mul_view_trans_block_ctl                          &
     &         (id_control, hd_block, qmat_file_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(modeview_and_fname_ctl), intent(inout) :: qmat_file_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      qmat_file_ctl%q_mat%i_view_transform = 0
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_block)) then
          write(*,'(a)', ADVANCE='NO') ' is read from file... '
          qmat_file_ctl%fname_qmat_ctl = third_word(c_buf)
          call read_control_modelview_file(id_control+2,                &
     &        qmat_file_ctl%fname_qmat_ctl, qmat_file_ctl%q_mat)
        end if
        if(check_begin_flag(c_buf, hd_block)) then
          write(*,*) trim(hd_block), ' is included'
          qmat_file_ctl%fname_qmat_ctl = 'NO_FILE'
          call read_view_transfer_ctl(id_control, hd_block,             &
     &                                qmat_file_ctl%q_mat, c_buf)
        end if
      end do
      qmat_file_ctl%q_mat%i_view_transform = 1
!
      end subroutine read_mul_view_trans_block_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_mul_view_trans_ctl(mul_qmats_c)
!
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_dup_view_transfer_ctl
!
      type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
!
      integer(kind = kint) :: i, num
!
!
      call calypso_mpi_bcast_one_int(mul_qmats_c%num_mul_qmats_c, 0)
      if(mul_qmats_c%num_mul_qmats_c .gt. 0 .and. my_rank .gt. 0) then
        num = mul_qmats_c%num_mul_qmats_c
        allocate(mul_qmats_c%qmat_file_ctl(num))
      end if
!
      do i = 1, mul_qmats_c%num_mul_qmats_c
        call calypso_mpi_bcast_character                                &
     &     (mul_qmats_c%qmat_file_ctl(i)%fname_qmat_ctl,                &
     &      cast_long(kchara), 0)
!
        call bcast_view_transfer_ctl                                    &
     &     (mul_qmats_c%qmat_file_ctl(i)%q_mat)
      end do
!
      end subroutine bcast_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_mul_view_trans_ctl(mul_qmats_c)
!
      type(multi_modeview_ctl), intent(inout) :: mul_qmats_c
!
      type(multi_modeview_ctl) :: tmp_mul_qmats
!
!
      tmp_mul_qmats%num_mul_qmats_c = mul_qmats_c%num_mul_qmats_c
      call alloc_multi_modeview_ctl(tmp_mul_qmats)
      call copy_mul_view_trans_ctl(tmp_mul_qmats%num_mul_qmats_c,       &
     &    mul_qmats_c%qmat_file_ctl, tmp_mul_qmats%qmat_file_ctl)
!
      call dealloc_multi_modeview_ctl(mul_qmats_c)
!
      mul_qmats_c%num_mul_qmats_c = tmp_mul_qmats%num_mul_qmats_c + 1
      call alloc_multi_modeview_ctl(mul_qmats_c)
!
      call copy_mul_view_trans_ctl(tmp_mul_qmats%num_mul_qmats_c,       &
     &    tmp_mul_qmats%qmat_file_ctl, mul_qmats_c%qmat_file_ctl(1))
!
      call dealloc_multi_modeview_ctl(tmp_mul_qmats)
!
      end subroutine append_mul_view_trans_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_mul_view_trans_ctl(org_mul_qmats_c,                &
     &                                  new_mul_qmats_c)
!
      type(multi_modeview_ctl), intent(in) :: org_mul_qmats_c
      type(multi_modeview_ctl), intent(inout) :: new_mul_qmats_c
!
!
      new_mul_qmats_c%num_mul_qmats_c                                   &
     &     = org_mul_qmats_c%num_mul_qmats_c
      call alloc_multi_modeview_ctl(new_mul_qmats_c)
      call copy_mul_view_trans_ctl(org_mul_qmats_c%num_mul_qmats_c,     &
     &                             org_mul_qmats_c%qmat_file_ctl,       &
     &                             new_mul_qmats_c%qmat_file_ctl)
!
      end subroutine dup_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mul_view_trans_ctl                                &
     &         (num_qmat, org_qmat_ctl, new_qmat_ctl)
!
      integer(kind = kint), intent(in) :: num_qmat
      type(modeview_and_fname_ctl), intent(in)                          &
     &                             :: org_qmat_ctl(num_qmat)
      type(modeview_and_fname_ctl), intent(inout)                       &
     &                             :: new_qmat_ctl(num_qmat)
!
      integer(kind = kint) :: i
!
      do i = 1, num_qmat
        call dup_view_transfer_ctl(org_qmat_ctl(i)%q_mat,               &
     &                             new_qmat_ctl(i)%q_mat)
        new_qmat_ctl(i)%fname_qmat_ctl = org_qmat_ctl(i)%fname_qmat_ctl
      end do
!
      end subroutine copy_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mul_view_trans_ctl(num_qmat, qmat_file_ctl)
!
      integer(kind = kint), intent(in) :: num_qmat
      type(modeview_and_fname_ctl), intent(inout)                       &
     &                             :: qmat_file_ctl(num_qmat)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_qmat
        call dealloc_view_transfer_ctl(qmat_file_ctl(i)%q_mat)
      end do
!
      end subroutine dealloc_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_view_transfers
