!itp_table_data_IO_b.f90
!      module itp_table_data_IO_b
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!      subroutine write_interpolate_table_org_b(id_file, my_rank)
!      subroutine write_interpolate_coefs_org_b(id_file)
!
!      subroutine read_interpolate_domain_org_b(id_file, n_rank)
!      subroutine read_interpolate_table_org_b(id_file)
!      subroutine read_interpolate_coefs_org_b(id_file)
!
!      subroutine write_interpolate_table_dest_b(id_file, my_rank)
!      subroutine write_interpolate_coefs_dest_b(id_file)
!
!      subroutine read_interpolate_domain_dest_b(id_file, n_rank)
!      subroutine read_interpolate_table_dest_b(id_file)
!      subroutine read_interpolate_coefs_dest_b(id_file)
!
      module itp_table_data_IO_b
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_org_b(id_file, my_rank)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file) my_rank
      write(id_file) num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
        write(id_file) id_dest_domain_IO(1:num_dest_domain_IO)
        write(id_file) istack_nod_table_org_IO(1:num_dest_domain_IO)
        write(id_file)                                                  &
     &        IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
      end if
!
      end subroutine write_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_org_b(id_file)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, num
!
      if (num_dest_domain_IO .eq. 0) return
        write(id_file) istack_itp_type_org_IO(0:4)
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
      subroutine read_interpolate_domain_org_b(id_file, n_rank)
!
      use m_interpolate_table_org_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
!
!
      read(id_file) n_rank
      read(id_file) num_dest_domain_IO
!
      if (num_dest_domain_IO .gt. 0) then
        call allocate_itp_num_org_IO
        read(id_file) id_dest_domain_IO(1:num_dest_domain_IO)
      end if
!
      end subroutine read_interpolate_domain_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_org_b(id_file)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file
!
!
      if (num_dest_domain_IO .eq. 0) return
!
        read(id_file) istack_nod_table_org_IO(1:num_dest_domain_IO)
        IO_itp_org%ntot_table_org                                       &
     &        = istack_nod_table_org_IO(num_dest_domain_IO)
!
        call alloc_itp_table_org(IO_itp_org)
        read(id_file)                                                   &
     &        IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
!
      end subroutine read_interpolate_table_org_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_org_b(id_file)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, num
!
!
      if (num_dest_domain_IO .eq. 0) return
        read(id_file) istack_itp_type_org_IO(0:4)
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
      subroutine write_interpolate_table_dest_b(id_file, my_rank)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file) my_rank
      write(id_file) num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
        write(id_file) id_org_domain_IO(1:num_org_domain_IO)
!
        write(id_file) istack_table_dest_IO(1:num_org_domain_IO)
        write(id_file) inod_dest_IO(1:ntot_table_dest_IO)
      end if
!
      end subroutine write_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_dest_b(id_file)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
      if (num_org_domain_IO .eq. 0) return
!
        write(id_file)                                                  &
     &          istack_table_wtype_dest_IO(0:4*num_org_domain_IO)
!
        write(id_file) inod_global_dest_IO(1:ntot_table_dest_IO)
        write(id_file) iele_orgin_IO(1:ntot_table_dest_IO)
        write(id_file) itype_inter_dest_IO(1:ntot_table_dest_IO)
        do i = 1, 3
          write(id_file) coef_inter_dest_IO(1:ntot_table_dest_IO,i)
        end do
!
      end subroutine write_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_dest_b(id_file, n_rank)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
!
!
      read(id_file) n_rank
      read(id_file) num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
        call allocate_itp_num_dst_IO
        read(id_file) id_org_domain_IO(1:num_org_domain_IO)
      end if
!
      end subroutine read_interpolate_domain_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_dest_b(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
!
      if (num_org_domain_IO .eq. 0) return
!
        read(id_file) istack_table_dest_IO(1:num_org_domain_IO)
        ntot_table_dest_IO = istack_table_dest_IO(num_org_domain_IO)
!
        call allocate_itp_nod_dst_IO
        read(id_file) inod_dest_IO(1:ntot_table_dest_IO)
!
      end subroutine read_interpolate_table_dest_b
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_dest_b(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i
!
!
      if (num_org_domain_IO .eq. 0) return
!
        read(id_file) istack_table_wtype_dest_IO(0:num_org_domain_IO)
        ntot_table_dest_IO                                              &
     &        = istack_table_wtype_dest_IO(4*num_org_domain_IO)
!
        call allocate_itp_coefs_dst_IO
!
        read(id_file) inod_global_dest_IO(1:ntot_table_dest_IO)
        read(id_file) iele_orgin_IO(1:ntot_table_dest_IO)
        read(id_file) itype_inter_dest_IO(1:ntot_table_dest_IO)
        do i = 1, 3
          read(id_file) coef_inter_dest_IO(1:ntot_table_dest_IO,i)
        end do
!
      end subroutine read_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      end module itp_table_data_IO_b
