!
      program pick_point_node_monitor
!
!
      use m_precision
!
      use set_parallel_file_name
!
      implicit none
!
      real(kind = kreal) :: time
      real(kind = kreal) :: iflag_init = 0
      real(kind = kreal) :: xx(3)
      integer(kind = kint) :: my_rank, inod_2_pick, i_step_MHD
      integer(kind = kint) :: num_monitor_local
!
      integer(kind = kint) :: num_field_monitor
      integer(kind = kint) :: ntot_comp_monitor
      integer(kind = kint), allocatable :: num_nod_component(:)
      real(kind = kreal), allocatable :: d_nod(:)
      character(len=kchara), allocatable :: phys_name_monitor(:)
      integer(kind = kint), parameter :: id_monitor_file = 13
      integer(kind = kint), parameter :: pick_monitor_file_code = 14
      character(len=kchara) :: monitor_header = 'node'
      character(len=kchara), parameter :: picked_header = 'picked_node'
      character(len=kchara) :: nod_monitor_file_name
      character(len=kchara) :: picked_monitor_file_name
!
!
      integer(kind = kint) :: i, nd, inod
      character(len=kchara) :: tmpchara
      character(len=kchara) :: fname_tmp, fname_tmp2
!
!
      write(*,*) 'input domian ID'
      read(*,*) my_rank
      write(*,*) 'input local node ID'
      read(*,*) inod_2_pick
!
!
      do
        write(*,*) 'input monitor file header ("END" to finish)'
        read(*,*) monitor_header
!
        if(monitor_header .eq. 'end' .or. monitor_header .eq. 'END')    &
     &    go to 98
!
        call add_int_suffix(my_rank, monitor_header, fname_tmp)
        call add_dat_extension(fname_tmp, nod_monitor_file_name)
!
        open(id_monitor_file, file=nod_monitor_file_name, err=98)
        read(id_monitor_file,*) num_monitor_local
        read(id_monitor_file,*) tmpchara
        read(id_monitor_file,*) tmpchara, num_field_monitor
!
        allocate(num_nod_component(num_field_monitor))
        allocate(phys_name_monitor(num_field_monitor))
!
        read(id_monitor_file,*) tmpchara,                               &
     &        num_nod_component(1:num_field_monitor)
        do i = 1, num_field_monitor
          read(id_monitor_file,*) phys_name_monitor(i)
        end do
!
        if(iflag_init .eq. 0) then
          call add_int_suffix(inod_2_pick, picked_header, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname_tmp2)
          call add_dat_extension(fname_tmp2, picked_monitor_file_name)
!
          open(pick_monitor_file_code, file=picked_monitor_file_name)
          write(pick_monitor_file_code,*) num_monitor_local
          write(pick_monitor_file_code,*) 'ID step time x y z '
          write(pick_monitor_file_code,1001)  num_field_monitor
          write(pick_monitor_file_code,1002)                            &
     &        num_nod_component(1:num_field_monitor)
 1001     format('number_of_fields: ',i10)
 1002     format('number_of_components: ',200i3)
!
          do i = 1, num_field_monitor
            write(pick_monitor_file_code,*) trim(phys_name_monitor(i))
          end do
!
          ntot_comp_monitor = 0
          do nd = 1, num_field_monitor
            ntot_comp_monitor = ntot_comp_monitor                       &
     &                            + num_nod_component(nd)
          end do
!
          allocate(d_nod(ntot_comp_monitor))
          iflag_init = 1
        end if
!
        deallocate(num_nod_component)
        deallocate(phys_name_monitor)
!
        do
          do i = 1, num_monitor_local
            read(id_monitor_file,*,end=99,err=99)                       &
     &        i_step_MHD, inod, time, xx(1:3),                          &
     &        d_nod(1:ntot_comp_monitor)
!
            if(inod .eq. inod_2_pick) then
              write(pick_monitor_file_code,'(2i8,50e16.6)')             &
     &        i_step_MHD, inod, time, xx(1:3),                          &
     &        d_nod(1:ntot_comp_monitor)
            end if
!
          end do
        end do
  99    continue
!
        close(id_monitor_file)
      end do
!
  98  continue
      close(pick_monitor_file_code)
      deallocate(d_nod)
!
      stop
      end

