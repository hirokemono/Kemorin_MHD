!>@file  itp_table_org_data_IO_b.f90
!!       module itp_table_org_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2006
!!
!> @brief Binary data IO for interpolation
!!
!!@verbatim
!!      subroutine write_interpolate_table_org_b                        &
!!     &         (id_rank, IO_itp_org, bbuf)
!!      subroutine write_interpolate_idx_org_b(IO_itp_org, bbuf)
!!      subroutine write_interpolate_coefs_org_b(IO_itp_org, bbuf)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine read_interpolate_domain_org_b                        &
!!     &         (bbuf, n_rank, IO_itp_org)
!!      subroutine read_interpolate_table_org_b(bbuf, IO_itp_org)
!!      subroutine read_interpolate_idx_org_b(bbuf, IO_itp_org)
!!      subroutine read_interpolate_coefs_org_b(bbuf, IO_itp_org)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!@endverbatim
!
      module itp_table_org_data_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_org_b                          &
     &         (id_rank, IO_itp_org, bbuf)
!
      use t_interpolate_tbl_org
!
      integer, intent(in) :: id_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: irank_write
!
!
      irank_write = int(id_rank,KIND(irank_write))
      call write_one_integer_b(irank_write, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(IO_itp_org%num_dest_domain, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      if (IO_itp_org%num_dest_domain .le. 0) return
!
      num64 = IO_itp_org%num_dest_domain
      call write_mul_integer_b                                          &
     &   (num64, IO_itp_org%id_dest_domain, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_integer_stack_b                                        &
     &   (num64, IO_itp_org%istack_nod_tbl_org, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call write_mul_integer_b                                          &
     &   (num64, IO_itp_org%inod_itp_send, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_idx_org_b(IO_itp_org, bbuf)
!
      use t_interpolate_tbl_org
!
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
      num64 = ifive
      call write_mul_integer_b                                          &
     &   (num64, IO_itp_org%istack_itp_type_org(0:4), bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call write_mul_int8_b                                             &
     &  (num64, IO_itp_org%inod_gl_dest_4_org, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_mul_integer_b                                          &
     &  (num64, IO_itp_org%iele_org_4_org, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_mul_integer_b                                          &
     &  (num64, IO_itp_org%itype_inter_org, bbuf)
!
      end subroutine write_interpolate_idx_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_org_b(IO_itp_org, bbuf)
!
      use t_interpolate_tbl_org
!
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_2d_vector_b(cast_long(IO_itp_org%ntot_table_org),      &
     &    ithree, IO_itp_org%coef_inter_org, bbuf)
!
      end subroutine write_interpolate_coefs_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org_b                          &
     &         (bbuf, n_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_one_integer_b(bbuf, n_rank)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_one_integer_b(bbuf, IO_itp_org%num_dest_domain)
      if(bbuf%ierr_bin .gt. 0) return
!
      call alloc_itp_num_org(np_smp, IO_itp_org)
      if (IO_itp_org%num_dest_domain .le. 0) return
!
      call read_mul_integer_b                                           &
     &   (bbuf, cast_long(IO_itp_org%num_dest_domain),                  &
     &    IO_itp_org%id_dest_domain)
      if(bbuf%ierr_bin .gt. 0) return
!
      end subroutine read_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org_b(bbuf, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .le. 0) return
!
      call read_integer_stack_b                                         &
     &   (bbuf, cast_long(IO_itp_org%num_dest_domain),                  &
     &    IO_itp_org%istack_nod_tbl_org, IO_itp_org%ntot_table_org)
      if(bbuf%ierr_bin .gt. 0) return
!
      call alloc_itp_table_org(IO_itp_org)
      call read_mul_integer_b                                           &
     &   (bbuf, cast_long(IO_itp_org%ntot_table_org),                   &
     &   IO_itp_org%inod_itp_send)
!
      end subroutine read_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_idx_org_b(bbuf, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
      call read_mul_integer_b(bbuf, cast_long(ifive),                   &
     &    IO_itp_org%istack_itp_type_org(0:4))
      if(bbuf%ierr_bin .gt. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call read_mul_int8_b                                              &
     &   (bbuf, num64, IO_itp_org%inod_gl_dest_4_org)
      if(bbuf%ierr_bin .ne. 0) return
      call read_mul_integer_b                                           &
     &   (bbuf, num64, IO_itp_org%iele_org_4_org)
      if(bbuf%ierr_bin .gt. 0) return
      call read_mul_integer_b                                           &
     &   (bbuf, num64, IO_itp_org%itype_inter_org)
!
      end subroutine read_interpolate_idx_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_org_b(bbuf, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
      call read_2d_vector_b                                             &
     &   (bbuf, cast_long(IO_itp_org%ntot_table_org), ithree,           &
     &    IO_itp_org%coef_inter_org)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine read_interpolate_coefs_org_b
!
!-----------------------------------------------------------------------
!
      end module itp_table_org_data_IO_b
