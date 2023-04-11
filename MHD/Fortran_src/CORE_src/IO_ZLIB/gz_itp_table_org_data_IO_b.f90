!>@file  gz_itp_table_org_data_IO_b.f90
!!       module gz_itp_table_org_data_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2006
!!
!> @brief Binary data IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_itp_table_org_b                             &
!!     &         (FPz_f, id_rank, IO_itp_org, zbuf)
!!      subroutine write_gz_itp_idx_org_b(FPz_f, IO_itp_org, zbuf)
!!      subroutine write_gz_itp_coefs_org_b(FPz_f, IO_itp_org, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_gz_itp_domain_org_b                             &
!!     &         (FPz_f, zbuf, n_rank, IO_itp_org)
!!      subroutine read_gz_itp_table_org_b(FPz_f, zbuf, IO_itp_org)
!!      subroutine read_gz_itp_idx_org_b(FPz_f, zbuf, IO_itp_org)
!!      subroutine read_gz_itp_coefs_org_b(FPz_f, zbuf, IO_itp_org)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!@endverbatim
!
      module gz_itp_table_org_data_IO_b
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
      subroutine write_gz_itp_table_org_b                               &
     &         (FPz_f, id_rank, IO_itp_org, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
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
     &   (FPz_f, IO_itp_org%num_dest_domain, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      if (IO_itp_org%num_dest_domain .le. 0) return
!
      num64 = IO_itp_org%num_dest_domain
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_org%id_dest_domain, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_integer_stack_b                                     &
     &   (FPz_f, num64, IO_itp_org%istack_nod_tbl_org, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_org%inod_itp_send, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine write_gz_itp_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_idx_org_b(FPz_f, IO_itp_org, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
      num64 = ifive
      call gz_write_mul_integer_b                                       &
     &   (FPz_f, num64, IO_itp_org%istack_itp_type_org(0:4), zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call gz_write_mul_int8_b                                          &
     &   (FPz_f, num64, IO_itp_org%inod_gl_dest_4_org, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_write_mul_integer_b                                       &
     &  (FPz_f, num64, IO_itp_org%iele_org_4_org, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine write_gz_itp_idx_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_org_b(FPz_f, IO_itp_org, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call gz_write_mul_integer_b                                       &
     &  (FPz_f, num64, IO_itp_org%itype_inter_org, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_write_2d_vector_b                                         &
     &   (FPz_f, cast_long(IO_itp_org%ntot_table_org),                  &
     &    ithree, IO_itp_org%coef_inter_org, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine write_gz_itp_coefs_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_org_b                               &
     &         (FPz_f, zbuf, n_rank, IO_itp_org)
!
      use skip_comment_f
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_read_one_integer_b(FPz_f, zbuf, n_rank)
      if(zbuf%ierr_zlib .gt. 0) return
!
      call gz_read_one_integer_b                                        &
     &   (FPz_f, zbuf, IO_itp_org%num_dest_domain)
      if(zbuf%ierr_zlib .gt. 0) return
!
      call alloc_itp_num_org(np_smp, IO_itp_org)
      if (IO_itp_org%num_dest_domain .le. 0) return
!
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, cast_long(IO_itp_org%num_dest_domain),           &
     &    IO_itp_org%id_dest_domain)
      if(zbuf%ierr_zlib .gt. 0) return
!
      end subroutine read_gz_itp_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_org_b(FPz_f, zbuf, IO_itp_org)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .le. 0) return
!
      call gz_read_integer_stack_b                                      &
     &   (FPz_f, zbuf, cast_long(IO_itp_org%num_dest_domain),           &
     &    IO_itp_org%istack_nod_tbl_org, IO_itp_org%ntot_table_org)
      if(zbuf%ierr_zlib .gt. 0) return
!
      call alloc_itp_table_org(IO_itp_org)
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, cast_long(IO_itp_org%ntot_table_org),            &
     &   IO_itp_org%inod_itp_send)
!
      end subroutine read_gz_itp_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_idx_org_b(FPz_f, zbuf, IO_itp_org)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
      call gz_read_mul_integer_b(FPz_f, zbuf, cast_long(ifive),         &
     &    IO_itp_org%istack_itp_type_org(0:4))
      if(zbuf%ierr_zlib .gt. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call gz_read_mul_int8_b                                           &
     &   (FPz_f, zbuf, num64, IO_itp_org%inod_gl_dest_4_org)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, num64, IO_itp_org%iele_org_4_org)
!
      end subroutine read_gz_itp_idx_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_org_b(FPz_f, zbuf, IO_itp_org)
!
      character, pointer, intent(in) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
      integer(kind = kint_gl) :: num64
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
      num64 = IO_itp_org%ntot_table_org
      call gz_read_mul_integer_b                                        &
     &   (FPz_f, zbuf, num64, IO_itp_org%itype_inter_org)
      if(zbuf%ierr_zlib .gt. 0) return
!
      call gz_read_2d_vector_b                                          &
     &   (FPz_f, zbuf, cast_long(IO_itp_org%ntot_table_org), ithree,    &
     &    IO_itp_org%coef_inter_org)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine read_gz_itp_coefs_org_b
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_org_data_IO_b
