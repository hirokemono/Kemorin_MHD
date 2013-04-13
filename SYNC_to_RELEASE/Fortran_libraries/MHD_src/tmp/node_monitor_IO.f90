!
!      module node_monitor_IO
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2007
!
!      subroutine allocate_monitor_group
!      subroutine allocate_monitor_local
!      subroutine deallocate_monitor_local
!
!      subroutine close_node_monitor_file
!
!      subroutine set_local_node_id_4_monitor
!      subroutine output_monitor_control
!      subroutine skip_monitor_data
!
      module node_monitor_IO
!
      use m_precision
!
      use m_control_parameter
      use m_file_control_parameter
!
      implicit none
!
      integer (kind=kint) :: num_monitor
      character (len=kchara), allocatable :: monitor_grp(:)
!
      integer (kind=kint) :: num_monitor_local
      integer (kind=kint), allocatable :: monitor_local(:)
!
      integer (kind = kint) :: num_field_monitor
      integer (kind = kint) :: ntot_comp_monitor
      integer (kind = kint), allocatable :: num_comp_phys_monitor(:)
      character (len = kchara), allocatable :: phys_name_monitor(:)
!
      private :: num_monitor_local, monitor_local
      private :: num_comp_phys_monitor, phys_name_monitor
      private :: allocate_monitor_local
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_monitor_group
!
!
      allocate( monitor_grp(num_monitor) )
!
      end subroutine allocate_monitor_group
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_monitor_local
!
!
      allocate( monitor_local(num_monitor_local) )
      if(num_monitor_local .gt. 0) monitor_local = 0
!
      end subroutine allocate_monitor_local
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_monitor_local
!
!
      deallocate(monitor_grp, monitor_local)
!
      end subroutine deallocate_monitor_local
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_open_node_monitor_file(my_rank)
!
      use m_node_phys_data
      use m_geometry_data
      use set_parallel_file_name
!
      integer (kind=kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
      integer (kind=kint) :: i, j
!
!
      allocate(num_comp_phys_monitor(num_field_monitor))
      allocate(phys_name_monitor(num_field_monitor))
!
      j = 0
      do i = 1, num_nod_phys
        if (iflag_nod_fld_monitor(i) .eq. 1 ) then
          j = j + 1
          num_comp_phys_monitor(j) = num_nod_component(i)
          phys_name_monitor(j) = phys_nod_name(i)
        end if
      end do
!
!
      call add_int_suffix(my_rank, node_monitor_head, fname_tmp)
      call add_dat_extension(fname_tmp, nod_monitor_file_name)
!
      if (num_monitor .ne. 0 .and. num_monitor_local .ne. 0) then
        open(id_monitor_file, file=nod_monitor_file_name       &
     &   ,status='replace')
        write(id_monitor_file,'(a)') num_monitor_local
        write(id_monitor_file,'(a)') 'ID step time x y z '
        write(id_monitor_file,1001)  num_field_monitor
        write(id_monitor_file,1002)                            &
     &        num_nod_component(1:num_field_monitor)
 1001   format('number_of_fields: ',i10)
 1002   format('number_of_components: ',200i3)
!
        do i = 1, num_field_monitor
          write(id_monitor_file,*) trim(phys_name_monitor(i))
        end do
      end if
!
      deallocate(num_comp_phys_monitor)
      deallocate(phys_name_monitor)
!
      end subroutine s_open_node_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine close_node_monitor_file
!
      if (num_monitor .ne. 0 .and. num_monitor_local .ne. 0) then
        close(id_monitor_file)
      end if
!
      end subroutine close_node_monitor_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_local_node_id_4_monitor
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_node_group
!
      integer (kind = kint) :: i, k, inum
!
!
!  count number of local nodes for monitor
!
      num_monitor_local = 0
!
      if ( num_monitor .gt. 0 ) then
        do i=1, num_bc
!
          do inum = 1, num_monitor
            if (bc_name(i) .eq. monitor_grp(inum)) then
               num_monitor_local = num_monitor_local                    &
     &                            + bc_istack(i)-bc_istack(i-1)
               exit
            end if
          end do
        end do
      end if
!
!   allocate local node ID
      call allocate_monitor_local
!
      if (num_monitor_local .eq. 0) return
!
      num_monitor_local = 0
      do i=1, num_bc
        do inum = 1, num_monitor
          if (bc_name(i) .eq. monitor_grp(inum)) then
            do k= bc_istack(i-1)+1, bc_istack(i)
              if( bc_item(k) .le. internal_node ) then 
                num_monitor_local = num_monitor_local + 1
                monitor_local(num_monitor_local) = bc_item(k)
              end if
            end do
            exit
          end if
        end do
      end do
!
      end subroutine set_local_node_id_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine output_monitor_control
!
      use m_parallel_var_dof
      use m_geometry_data
      use m_geometry_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_t_step_parameter
!
      integer (kind = kint) :: i, ii, inod, i_fld, ist, ied
!
!
      if (i_step_output_monitor .eq. 0) return
!
! ----------   time evolution for time
!
      ii = mod(istep_max_dt, i_step_output_monitor)
      if ( ii .eq. 0 ) then
!
        if (num_monitor .ne. 0 .and. num_monitor_local .ne. 0) then
!
          do i = 1, num_monitor_local
            inod = monitor_local(i)
            write(id_monitor_file,'(2i10,1pe25.15e3)',            &
     &             advance='NO') i_step_MHD, inod, time
            write(id_monitor_file,'(1p3e25.15e3)',                &
     &             advance='NO') xx(inod,1:3)
            do i_fld = 1, num_nod_phys
              if(iflag_nod_fld_monitor(i_fld) .gt. 0) then
                ist = istack_nod_component(i_fld-1) + 1
                ied = istack_nod_component(i_fld)
                write(id_monitor_file,'(1p6E25.15e3)',            &
     &             advance='NO')  d_nod(inod,ist:ied)
              end if
            end do
            write(id_monitor_file,'(a)') ''
          end do
!
        end if
      end if
!
      end subroutine output_monitor_control
!
!  ---------------------------------------------------------------------
!
      subroutine skip_monitor_data
!
      use m_node_phys_data
      use m_t_step_parameter
!
      integer (kind = kint) :: i, k, itmp
      integer (kind = kint) :: i_read_step
      real(kind = kreal) :: rtmp
!
!
      if (num_monitor .ne. 0 .and. num_monitor_local .ne. 0) then
        do
          do i = 1, num_monitor_local
           read(id_monitor_file,*) i_read_step, itmp, rtmp,       &
     &         (rtmp,k=1,3), (rtmp,k=1,ntot_comp_monitor)
          end do
          if (i_read_step.ge.i_step_init) exit
        end do
      end if
!
      end subroutine skip_monitor_data
!
!  ---------------------------------------------------------------------
!
      end module node_monitor_IO
