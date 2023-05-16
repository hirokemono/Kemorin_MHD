!>@file  gz_itp_table_dest_data_IO_b.f90
!!       module gz_itp_table_dest_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2006
!!
!> @brief Binary data IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_itp_table_dest_b                            &
!!     &         (FPz_f, id_rank, IO_itp_dest, zbuf)
!!      subroutine write_gz_itp_idx_dest_b                              &
!!     &         (FPz_f, IO_itp_dest, IO_itp_c_dest, zbuf)
!!      subroutine write_gz_itp_coefs_dest_b                            &
!!     &         (FPz_f, IO_itp_dest, IO_itp_c_dest, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!
!!      subroutine read_gz_itp_domain_dest_b                            &
!!     &         (FPz_f, zbuf, n_rank, IO_itp_dest)
!!      subroutine read_gz_itp_table_dest_b(FPz_f, zbuf, IO_itp_dest)
!!      subroutine read_gz_itp_idx_dest_b                               &
!!     &         (FPz_f, zbuf, IO_itp_dest, IO_itp_c_dest)
!!      subroutine read_gz_itp_coefs_dest_b                             &
!!     &         (FPz_f, zbuf, IO_itp_dest, IO_itp_c_dest)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module gz_itp_table_dest_data_IO_b
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_interpolate_tbl_org
      use t_buffer_4_gzip
      use gz_binary_IO
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
      subroutine write_gz_itp_table_dest_b                              &
     &         (FPz_f, id_rank, IO_itp_dest, zbuf)
!
      use t_interpolate_tbl_dest
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: irank_write
!
!
      irank_write = int(id_rank,KIND(irank_write))
      call gz_write_one_integer_b(FPz_f, irank_write, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_one_integer_b                                       &
     &   (FPz_f, IO_itp_dest%num_org_domain, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      if (IO_itp_dest%num_org_domain .le. 0) return
!
      num64 = IO_itp_dest%num_org_domain
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_dest%id_org_domain, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_integer_stack_b                                     &
     &   (FPz_f, num64, IO_itp_dest%istack_nod_tbl_dest, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      num64 = IO_itp_dest%ntot_table_dest
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_dest%inod_dest_4_dest, zbuf)
!
      end subroutine write_gz_itp_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_idx_dest_b                              &
     &         (FPz_f, IO_itp_dest, IO_itp_c_dest, zbuf)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
      num64 = 4*IO_itp_dest%num_org_domain + 1
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_c_dest%istack_nod_tbl_wtype_dest, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      num64 = IO_itp_dest%ntot_table_dest
      call gz_write_mul_int8_b                                          &
     &   (FPz_f, num64, IO_itp_c_dest%inod_gl_dest, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_c_dest%iele_org_4_dest, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_c_dest%itype_inter_dest, zbuf)
!
      end subroutine write_gz_itp_idx_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_dest_b                              &
     &         (FPz_f, IO_itp_dest, IO_itp_c_dest, zbuf)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
!
      call gz_write_2d_vector_b                                         &
     &   (FPz_f, cast_long(IO_itp_dest%ntot_table_dest),                &
     &    ithree, IO_itp_c_dest%coef_inter_dest, zbuf)
!
      end subroutine write_gz_itp_coefs_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_dest_b                              &
     &         (FPz_f, zbuf, n_rank, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, n_rank)
      if(zbuf%ierr_zlib .gt. 0) return
!
      call gz_read_one_integer_b                                        &
     &   (FPz_f, zbuf, IO_itp_dest%num_org_domain)
      if(zbuf%ierr_zlib .gt. 0) return
!
      call alloc_itp_num_dest(IO_itp_dest)
!
      if (IO_itp_dest%num_org_domain .le. 0) return
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, cast_long(IO_itp_dest%num_org_domain),           &
     &    IO_itp_dest%id_org_domain)
!
      end subroutine read_gz_itp_domain_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_dest_b(FPz_f, zbuf, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(IO_itp_dest%num_org_domain .gt. 0) then
        call gz_read_integer_stack_b                                    &
     &     (FPz_f, zbuf, cast_long(IO_itp_dest%num_org_domain),         &
     &      IO_itp_dest%istack_nod_tbl_dest,                            &
     &      IO_itp_dest%ntot_table_dest)
        if(zbuf%ierr_zlib .gt. 0) return
      else
        IO_itp_dest%ntot_table_dest = 0
      end if
!
      call alloc_itp_table_dest(IO_itp_dest)
      if(IO_itp_dest%ntot_table_dest .le. 0) return
!
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, cast_long(IO_itp_dest%ntot_table_dest),          &
     &    IO_itp_dest%inod_dest_4_dest)
!
      end subroutine read_gz_itp_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_idx_dest_b                                 &
     &         (FPz_f, zbuf, IO_itp_dest, IO_itp_c_dest)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use skip_comment_f
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
      call alloc_itp_coef_stack                                         &
     &   (IO_itp_dest%num_org_domain, IO_itp_c_dest)
!
      num64 = 4*IO_itp_dest%num_org_domain + 1
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, num64, IO_itp_c_dest%istack_nod_tbl_wtype_dest)
      if(zbuf%ierr_zlib .gt. 0) return
!
      IO_itp_dest%ntot_table_dest                                       &
     &   = IO_itp_c_dest%istack_nod_tbl_wtype_dest(num64)
!
      call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)
!
      num64 = IO_itp_dest%ntot_table_dest
      call gz_read_mul_int8_b                                           &
     &   (FPz_f, zbuf, num64, IO_itp_c_dest%inod_gl_dest)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, num64, IO_itp_c_dest%iele_org_4_dest)
      if(zbuf%ierr_zlib .gt. 0) return
      call gz_read_mul_integer_b                                        &
        (FPz_f, zbuf, num64, IO_itp_c_dest%itype_inter_dest)
!
      end subroutine read_gz_itp_idx_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_dest_b                               &
     &         (FPz_f, zbuf, IO_itp_dest, IO_itp_c_dest)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use skip_comment_f
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
!
      call gz_read_2d_vector_b                                          &
     &   (FPz_f, zbuf, cast_long(IO_itp_dest%ntot_table_dest), ithree,  &
     &    IO_itp_c_dest%coef_inter_dest)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine read_gz_itp_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_dest_data_IO_b
