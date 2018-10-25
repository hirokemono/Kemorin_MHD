!itp_table_data_IO_b.f90
!      module itp_table_data_IO_b
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!>@file  itp_table_data_IO_b.f90
!!       module itp_table_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2006
!
!> @brief Binary data IO for interpolation
!!
!!@verbatim
!!      subroutine write_interpolate_table_org_b(my_rank, IO_itp_org)
!!      subroutine write_interpolate_coefs_org_b(IO_itp_org)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!
!!      subroutine read_interpolate_domain_org_b                        &
!!     &         (bin_flags, n_rank, IO_itp_org)
!!      subroutine read_interpolate_table_org_b(bin_flags, IO_itp_org)
!!      subroutine read_interpolate_coefs_org_b(bin_flags, IO_itp_org)
!!        type(file_IO_flags), intent(inout) :: bin_flags
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!
!!      subroutine write_interpolate_table_dest_b(my_rank, IO_itp_dest)
!!      subroutine write_interpolate_coefs_dest_b                       &
!!     &         (IO_itp_dest, IO_itp_c_dest)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!
!!      subroutine read_interpolate_domain_dest_b                       &
!!     &         (bin_flags, n_rank, IO_itp_dest)
!!      subroutine read_interpolate_table_dest_b(bin_flags, IO_itp_dest)
!!      subroutine read_interpolate_coefs_dest_b                        &
!!     &         (bin_flags, IO_itp_dest, IO_itp_c_dest)
!!        type(file_IO_flags), intent(inout) :: bin_flags
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module itp_table_data_IO_b
!
      use m_precision
      use m_machine_parameter
      use binary_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_org_b(my_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      call write_one_integer_b(my_rank)
      call write_one_integer_b(IO_itp_org%num_dest_domain)
!
      if (IO_itp_org%num_dest_domain .le. 0) return
      call write_mul_integer_b                                          &
     &  (IO_itp_org%num_dest_domain, IO_itp_org%id_dest_domain)
      call write_integer_stack_b                                        &
     &  (IO_itp_org%num_dest_domain, IO_itp_org%istack_nod_tbl_org)
      call write_mul_integer_b                                          &
     &  (IO_itp_org%ntot_table_org, IO_itp_org%inod_itp_send)
!
      end subroutine write_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_org_b(IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
      call write_mul_integer_b                                          &
     &   (ifive, IO_itp_org%istack_itp_type_org(0:4))
!
      call write_mul_integer_b                                          &
     &  (IO_itp_org%ntot_table_org, IO_itp_org%inod_gl_dest_4_org)
      call write_mul_integer_b                                          &
     &  (IO_itp_org%ntot_table_org, IO_itp_org%iele_org_4_org)
      call write_mul_integer_b                                          &
     &  (IO_itp_org%ntot_table_org, IO_itp_org%itype_inter_org)
      call write_2d_vector_b(IO_itp_org%ntot_table_org, ithree,         &
     &    IO_itp_org%coef_inter_org)
!
      end subroutine write_interpolate_coefs_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org_b                          &
     &         (bin_flags, n_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(file_IO_flags), intent(inout) :: bin_flags
!
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    n_rank, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_org%num_dest_domain, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      if (IO_itp_org%num_dest_domain .le. 0) return
      call alloc_itp_num_org(np_smp, IO_itp_org)
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_org%num_dest_domain, IO_itp_org%id_dest_domain,        &
     &    bin_flags%ierr_IO)
!
      end subroutine read_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org_b(bin_flags, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .le. 0) return
!
      call read_integer_stack_b(bin_flags%iflag_bin_swap,               &
     &    IO_itp_org%num_dest_domain, IO_itp_org%istack_nod_tbl_org,    &
     &    IO_itp_org%ntot_table_org, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_itp_table_org(IO_itp_org)
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_org%ntot_table_org, IO_itp_org%inod_itp_send,          &
     &    bin_flags%ierr_IO)
!
      end subroutine read_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_org_b(bin_flags, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    ifive, IO_itp_org%istack_itp_type_org(0:4),                   &
     &    bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_org%ntot_table_org, IO_itp_org%inod_gl_dest_4_org,     &
     &    bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_org%ntot_table_org, IO_itp_org%iele_org_4_org,         &
     &    bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_org%ntot_table_org, IO_itp_org%itype_inter_org,        &
     &    bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_2d_vector_b(bin_flags%iflag_bin_swap,                   &
     &    IO_itp_org%ntot_table_org, ithree,                            &
     &    IO_itp_org%coef_inter_org, bin_flags%ierr_IO)
!
      end subroutine read_interpolate_coefs_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_dest_b(my_rank, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
!
      call write_one_integer_b(my_rank)
      call write_one_integer_b(IO_itp_dest%num_org_domain)
!
      if (IO_itp_dest%num_org_domain .le. 0) return
      call write_mul_integer_b                                          &
     &  (IO_itp_dest%num_org_domain, IO_itp_dest%id_org_domain)
!
      call write_integer_stack_b                                        &
     &  (IO_itp_dest%num_org_domain, IO_itp_dest%istack_nod_tbl_dest)
!
      call write_mul_integer_b                                          &
     &  (IO_itp_dest%ntot_table_dest, IO_itp_dest%inod_dest_4_dest)
!
      end subroutine write_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_dest_b                         &
     &         (IO_itp_dest, IO_itp_c_dest)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: ncomp
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        ncomp = 4*IO_itp_dest%num_org_domain + 1
        call write_mul_integer_b                                        &
     &    (ncomp, IO_itp_c_dest%istack_nod_tbl_wtype_dest)
!
        call write_mul_integer_b                                        &
     &    (IO_itp_dest%ntot_table_dest, IO_itp_c_dest%inod_gl_dest)
        call write_mul_integer_b                                        &
     &    (IO_itp_dest%ntot_table_dest, IO_itp_c_dest%iele_org_4_dest)
        call write_mul_integer_b                                        &
     &    (IO_itp_dest%ntot_table_dest, IO_itp_c_dest%itype_inter_dest)
!
        call write_2d_vector_b(IO_itp_dest%ntot_table_dest,             &
     &      ithree, IO_itp_c_dest%coef_inter_dest)
!
      end subroutine write_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_dest_b                         &
     &         (bin_flags, n_rank, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(file_IO_flags), intent(inout) :: bin_flags
!
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    n_rank, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_dest%num_org_domain, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      if (IO_itp_dest%num_org_domain .le. 0) return
      call alloc_itp_num_dest(IO_itp_dest)
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_itp_dest%num_org_domain, IO_itp_dest%id_org_domain,        &
     &    bin_flags%ierr_IO)
!
      end subroutine read_interpolate_domain_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_dest_b(bin_flags, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        call read_integer_stack_b(bin_flags%iflag_bin_swap,             &
     &     IO_itp_dest%num_org_domain, IO_itp_dest%istack_nod_tbl_dest, &
     &     IO_itp_dest%ntot_table_dest, bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call alloc_itp_table_dest(IO_itp_dest)
        call read_mul_integer_b(bin_flags%iflag_bin_swap,               &
     &      IO_itp_dest%ntot_table_dest, IO_itp_dest%inod_dest_4_dest,  &
     &      bin_flags%ierr_IO)
!
      end subroutine read_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_dest_b                          &
     &         (bin_flags, IO_itp_dest, IO_itp_c_dest)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use skip_comment_f
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: ncomp
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        call alloc_itp_coef_stack                                       &
     &     (IO_itp_dest%num_org_domain, IO_itp_c_dest)
!
        ncomp = 4*IO_itp_dest%num_org_domain + 1
        call read_mul_integer_b(bin_flags%iflag_bin_swap,               &
     &      ncomp, IO_itp_c_dest%istack_nod_tbl_wtype_dest,             &
     &      bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
!
        IO_itp_dest%ntot_table_dest                                     &
     &     = IO_itp_c_dest%istack_nod_tbl_wtype_dest(ncomp)
!
        call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)
!
        call read_mul_integer_b(bin_flags%iflag_bin_swap,               &
     &     IO_itp_dest%ntot_table_dest, IO_itp_c_dest%inod_gl_dest,     &
     &     bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call read_mul_integer_b(bin_flags%iflag_bin_swap,               &
     &     IO_itp_dest%ntot_table_dest, IO_itp_c_dest%iele_org_4_dest,  &
     &     bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call read_mul_integer_b(bin_flags%iflag_bin_swap,               &
     &     IO_itp_dest%ntot_table_dest, IO_itp_c_dest%itype_inter_dest, &
     &     bin_flags%ierr_IO)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call read_2d_vector_b(bin_flags%iflag_bin_swap,                 &
     &      IO_itp_dest%ntot_table_dest, ithree,                        &
     &      IO_itp_c_dest%coef_inter_dest, bin_flags%ierr_IO)
!
      end subroutine read_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      end module itp_table_data_IO_b
