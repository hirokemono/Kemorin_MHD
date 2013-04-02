!interpolate_table_dest_IO.f90
!      module interpolate_table_dest_IO
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!      subroutine write_interpolate_table_dest(id_file, my_rank)
!      subroutine write_interpolate_coefs_dest(id_file)
!
!      subroutine read_interpolate_domain_dest(id_file, n_rank)
!      subroutine read_interpolate_table_dest(id_file)
!      subroutine read_interpolate_coefs_dest(id_file)
!
!      subroutine write_interpolate_table_dest_b(id_file, my_rank)
!      subroutine write_interpolate_coefs_dest_b(id_file)
!
!      subroutine read_interpolate_domain_dest_b(id_file, n_rank)
!      subroutine read_interpolate_table_dest_b(id_file)
!      subroutine read_interpolate_coefs_dest_b(id_file)
!
      module interpolate_table_dest_IO
!
      use m_precision
      use m_constants
!
      implicit none
!
      character(len=255) :: character_4_read
      private :: character_4_read
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_table_dest(id_file, my_rank)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file, my_rank
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  domain ID '
      write(id_file,'(a)') '!  number of domain of origin'
      write(id_file,'(a)') '!  originate domain IDs'
      write(id_file,'(a)') '!'
!
      write(id_file,'(i10)') my_rank
      write(id_file,'(i10)') num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
        write(id_file,'(10i8)')                                         &
     &          id_org_domain_IO(1:num_org_domain_IO)
      else
        write(id_file,*)
      end if
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  stack of originate domain'
      write(id_file,'(a)') '!  destination node ID'
      write(id_file,'(a)') '!'
!
      if (num_org_domain_IO .gt. 0) then
        write(id_file,'(8i10)')                                         &
              istack_table_dest_IO(1:num_org_domain_IO)
        write(id_file,'(8i10)') inod_dest_IO(1:ntot_table_dest_IO)
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine write_interpolate_coefs_dest(id_file)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, inod
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!  stack by interpolation type'
      write(id_file,'(a)') '!  target global node ID, '
      write(id_file,'(a)') '!  belonged local element ID '
      write(id_file,'(a)') '!  interpolation type ID '
      write(id_file,'(a)') '!  generalized position '
      write(id_file,'(a)') '!'
!
      if (num_org_domain_IO .gt. 0) then
        do i = 1, num_org_domain_IO
          write(id_file,'(8i10)')                                       &
     &                istack_table_wtype_dest_IO(4*i-3:4*i)
        end do
!
        do inod = 1, ntot_table_dest_IO
          write(id_file,'(3i10,1p3e23.12)')                             &
     &        inod_global_dest_IO(inod), iele_orgin_IO(inod),           &
     &        itype_inter_dest_IO(inod), coef_inter_dest_IO(inod,1:3)
        end do
!
      else
        write(id_file,*)
      end if
!
      end subroutine write_interpolate_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_domain_dest(id_file, n_rank)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: n_rank
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) n_rank
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) num_org_domain_IO
!
      if (num_org_domain_IO .gt. 0) then
!
        call allocate_itp_num_dst_IO
        read(id_file,*) id_org_domain_IO(1:num_org_domain_IO)
!
      end if
!
      end subroutine read_interpolate_domain_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_table_dest(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
!
      if (num_org_domain_IO .gt. 0) then
!
        call read_stack_array(character_4_read, id_file,                &
     &    num_org_domain_IO, istack_table_dest_IO)
        ntot_table_dest_IO = istack_table_dest_IO(num_org_domain_IO)
!
        call allocate_itp_nod_dst_IO
!
        read(id_file,*) inod_dest_IO(1:ntot_table_dest_IO)
!
      end if
!
      end subroutine read_interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_interpolate_coefs_dest(id_file)
!
      use m_interpolate_table_dest_IO
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint) :: i, inod
!
!
      if (num_org_domain_IO .gt. 0) then
!
        call read_stack_array(character_4_read, id_file,                &
     &      ifour, istack_table_wtype_dest_IO(0) )
        do i = 2, num_org_domain_IO
          read(id_file,*) istack_table_wtype_dest_IO(4*i-3:4*i)
        end do
        ntot_table_dest_IO                                              &
     &        = istack_table_wtype_dest_IO(4*num_org_domain_IO)
!
        call allocate_itp_coefs_dst_IO
!
        do inod = 1, ntot_table_dest_IO
          read(id_file,*) inod_global_dest_IO(inod),                    &
     &        iele_orgin_IO(inod), itype_inter_dest_IO(inod),           &
     &        coef_inter_dest_IO(inod,1:3)
        end do
!
      end if
!
      end subroutine read_interpolate_coefs_dest
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
!
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
      if (num_org_domain_IO .gt. 0) then
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
      end if
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
      if (num_org_domain_IO .gt. 0) then
!
        read(id_file) istack_table_dest_IO(1:num_org_domain_IO)
        ntot_table_dest_IO = istack_table_dest_IO(num_org_domain_IO)
!
        call allocate_itp_nod_dst_IO
        read(id_file) inod_dest_IO(1:ntot_table_dest_IO)
!
      end if
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
      if (num_org_domain_IO .gt. 0) then
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
      end if
!
      end subroutine read_interpolate_coefs_dest_b
!
!-----------------------------------------------------------------------
!
      end module interpolate_table_dest_IO
