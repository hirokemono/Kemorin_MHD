!itp_table_data_IO_b.f90
!      module itp_table_data_IO_b
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!!      subroutine write_interpolate_table_org_b                        &
!!     &         (id_file, my_rank, IO_itp_org)
!!      subroutine write_interpolate_coefs_org_b(id_file, IO_itp_org)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!
!!      subroutine read_interpolate_domain_org_b                        &
!!     &         (id_file, n_rank, IO_itp_org)
!!      subroutine read_interpolate_table_org_b(id_file, IO_itp_org)
!!      subroutine read_interpolate_coefs_org_b(id_file, IO_itp_org)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!
!!      subroutine write_interpolate_table_dest_b                       &
!!     &         (id_file, my_rank, IO_itp_dest)
!!      subroutine write_interpolate_coefs_dest_b                       &
!!     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!
!!      subroutine read_interpolate_domain_dest_b                       &
!!     &         (id_file, n_rank, IO_itp_dest)
!!      subroutine read_interpolate_table_dest_b(id_file, IO_itp_dest)
!!      subroutine read_interpolate_coefs_dest_b                        &
!!     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      module itp_table_data_IO_b
!
      use m_precision
      use m_machine_parameter
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
     &         (id_file, my_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: id_file, my_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      write(id_file) my_rank
      write(id_file) IO_itp_org%num_dest_domain
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        write(id_file)                                                  &
     &      IO_itp_org%id_dest_domain(1:IO_itp_org%num_dest_domain)
        write(id_file)                                                  &
     &      IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain)
        write(id_file)                                                  &
     &      IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
      end if
!
      end subroutine write_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_org_b(id_file, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
      integer(kind = kint) :: i, num
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
        write(id_file) IO_itp_org%istack_itp_type_org(0:4)
!
        num = IO_itp_org%ntot_table_org
        write(id_file) IO_itp_org%inod_gl_dest_4_org(1:num)
        write(id_file) IO_itp_org%iele_org_4_org(1:num)
        write(id_file) IO_itp_org%itype_inter_org(1:num)
        do i = 1, 3
          write(id_file) IO_itp_org%coef_inter_org(1:num,i)
        end do
!
      end subroutine write_interpolate_coefs_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_org_b                          &
     &         (id_file, n_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      read(id_file) n_rank
      read(id_file) IO_itp_org%num_dest_domain
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call alloc_itp_num_org(np_smp, IO_itp_org)
        read(id_file)                                                   &
     &        IO_itp_org%id_dest_domain(1:IO_itp_org%num_dest_domain)
      end if
!
      end subroutine read_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org_b(id_file, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
        IO_itp_org%istack_nod_tbl_org(0) = 0
        read(id_file)                                                   &
     &     IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain)
        IO_itp_org%ntot_table_org                                       &
     &     = IO_itp_org%istack_nod_tbl_org(IO_itp_org%num_dest_domain)
!
        call alloc_itp_table_org(IO_itp_org)
        read(id_file)                                                   &
     &        IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
!
      end subroutine read_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_org_b(id_file, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
      integer(kind = kint) :: i, num
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
        read(id_file) IO_itp_org%istack_itp_type_org(0:4)
!
        num = IO_itp_org%ntot_table_org
        read(id_file) IO_itp_org%inod_gl_dest_4_org(1:num)
        read(id_file) IO_itp_org%iele_org_4_org(1:num)
        read(id_file) IO_itp_org%itype_inter_org(1:num)
        do i = 1, 3
          read(id_file) IO_itp_org%coef_inter_org(1:num,i)
        end do
!
      end subroutine read_interpolate_coefs_org_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_dest_b                         &
     &         (id_file, my_rank, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      integer(kind = kint), intent(in) :: id_file, my_rank
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
!
!
      write(id_file) my_rank
      write(id_file) IO_itp_dest%num_org_domain
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        write(id_file)                                                  &
     &    IO_itp_dest%id_org_domain(1:IO_itp_dest%num_org_domain)
!
        write(id_file)                                                  &
     &    IO_itp_dest%istack_nod_tbl_dest(1:IO_itp_dest%num_org_domain)
        write(id_file)                                                  &
     &    IO_itp_dest%inod_dest_4_dest(1:IO_itp_dest%ntot_table_dest)
      end if
!
      end subroutine write_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_dest_b                         &
     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: i, num
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
!
        num = 4*IO_itp_dest%num_org_domain
        write(id_file)                                                  &
     &      IO_itp_c_dest%istack_nod_tbl_wtype_dest(0:num)
!
        num = IO_itp_dest%ntot_table_dest
        write(id_file) IO_itp_c_dest%inod_gl_dest(1:num)
        write(id_file) IO_itp_c_dest%iele_org_4_dest(1:num)
        write(id_file) IO_itp_c_dest%itype_inter_dest(1:num)
        do i = 1, 3
          write(id_file) IO_itp_c_dest%coef_inter_dest(1:num,i)
        end do
!
      end subroutine write_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_dest_b                         &
     &         (id_file, n_rank, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      read(id_file) n_rank
      read(id_file) IO_itp_dest%num_org_domain
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call alloc_itp_num_dest(IO_itp_dest)
        read(id_file)                                                   &
     &      IO_itp_dest%id_org_domain(1:IO_itp_dest%num_org_domain)
      end if
!
      end subroutine read_interpolate_domain_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_dest_b(id_file, IO_itp_dest)
!
      use t_interpolate_tbl_dest
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
!
        read(id_file)                                                   &
     &    IO_itp_dest%istack_nod_tbl_dest(1:IO_itp_dest%num_org_domain)
        IO_itp_dest%ntot_table_dest                                     &
     &    = IO_itp_dest%istack_nod_tbl_dest(IO_itp_dest%num_org_domain)
!
        call alloc_itp_table_dest(IO_itp_dest)
        read(id_file)                                                   &
     &     IO_itp_dest%inod_dest_4_dest(1:IO_itp_dest%ntot_table_dest)
!
      end subroutine read_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_dest_b                          &
     &         (id_file, IO_itp_dest, IO_itp_c_dest)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: i, num
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        call alloc_itp_coef_stack                                       &
     &     (IO_itp_dest%num_org_domain, IO_itp_c_dest)
!
        num = 4*IO_itp_dest%num_org_domain
        read(id_file)                                                   &
     &     IO_itp_c_dest%istack_nod_tbl_wtype_dest(0:num)
        IO_itp_dest%ntot_table_dest                                     &
     &     = IO_itp_c_dest%istack_nod_tbl_wtype_dest(num)
!
        call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)
!
        num = IO_itp_dest%ntot_table_dest
        read(id_file) IO_itp_c_dest%inod_gl_dest(1:num)
        read(id_file) IO_itp_c_dest%iele_org_4_dest(1:num)
        read(id_file) IO_itp_c_dest%itype_inter_dest(1:num)
        do i = 1, 3
          read(id_file) IO_itp_c_dest%coef_inter_dest(1:num,i)
        end do
!
      end subroutine read_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      end module itp_table_data_IO_b
