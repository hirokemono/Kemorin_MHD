!
      program distribute_para_itp_table
!
      use t_mesh_data
      use t_interpolate_table
!
      use m_ctl_data_gen_table
      use m_read_mesh_data
      use m_work_ditribute_itp
      use distribute_itp_tbl_4_para
      use set_parallel_mesh_in_1pe
      use itp_table_IO_select_4_zlib
      use copy_interpolate_type_IO
!
      implicit  none
!
      character(len = kchara) :: org_mesh_head =   "mesh_fine/in"
      character(len = kchara) :: dest_mesh_head =  "mesh_coase/in"
      character(len = kchara) :: table_file_head = "mesh/table"
      character(len = kchara)                                           &
     &             :: sgl_table_file_head = "single_itp_table"
!
      integer(kind = kint) :: nprocs_dest
      type(mesh_data), allocatable, save :: femmesh_dest(:)
!
      integer(kind = kint) :: nprocs_org
      type(mesh_data), allocatable, save :: femmesh_org(:)
!
      type(interpolate_table), save :: single_tbl
!
      integer(kind = kint) :: nprocs_table
      type(interpolate_table), allocatable, save :: para_tbl(:)
!
!  .......................
!
     integer(kind = kint) :: ierr, ip, my_rank
!
!
      call read_control_4_distribute_itp
      call set_control_4_dist_itp
!
      mesh_file_head = dest_mesh_head
      allocate( femmesh_dest(nprocs_dest) )
      call s_set_parallel_mesh_in_1pe(nprocs_dest, femmesh_dest)
!
      mesh_file_head = org_mesh_head
      allocate( femmesh_org(nprocs_org) )
      call s_set_parallel_mesh_in_1pe(nprocs_org, femmesh_org)
!
!
      table_file_header = sgl_table_file_head
      call sel_read_interpolate_table(izero, ierr)
      call copy_interpolate_types_from_IO(izero, single_tbl)
      call set_stack_tbl_org_smp_type(single_tbl%tbl_org)
      write(*,*) 'interplation table is loaded'
!
!
!
      call set_work_4_ditribute_itp(nprocs_org, nprocs_dest,            &
     &          femmesh_org, femmesh_dest, single_tbl)
!
      nprocs_table = max(nprocs_itp_org, nprocs_itp_dest)
      allocate( para_tbl(nprocs_table) )
!
      call const_parallel_itp_tbl(single_tbl, nprocs_table,  para_tbl)
!
!
      table_file_header = table_file_head
      write(*,*) 'table field header: ', trim(table_file_header)
      do ip = 1, nprocs_table
        my_rank = ip - 1
        call copy_interpolate_types_to_IO( para_tbl(ip) )
        call sel_write_interpolate_table(my_rank)
      end do
!
      stop 'Distibution finished'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_dist_itp
!
      use m_ctl_data_gen_table
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_read_mesh_data
      use m_file_format_switch
      use set_control_platform_data
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
!
      org_mesh_head = mesh_file_head
      nprocs_org = 1
      if(ndomain_ctl%iflag .gt. 0) nprocs_org = ndomain_ctl%intvalue
!
      nprocs_dest = 1
      if(num_new_domain_ctl%iflag .gt. 0) then
         nprocs_dest = num_new_domain_ctl%intvalue
      end if
!
      if (new_mesh_prefix%iflag .ne. 0) then
        dest_mesh_head = new_mesh_prefix%charavalue
      end if
!
      if (table_head_ctl%iflag .ne. 0) then
        table_file_head = table_head_ctl%charavalue
      end if
!
      if (single_itp_tbl_head_ctl%iflag .ne. 0) then
        sgl_table_file_head = single_itp_tbl_head_ctl%charavalue
      end if
!
      call choose_file_format                                           &
     &   (fmt_itp_table_file_ctl, ifmt_itp_table_file)
!
      if (iflag_debug.eq.1) then
        write(*,*) 'np_smp', np_smp, np_smp
        write(*,*) 'org_mesh_head: ',   trim(org_mesh_head)
        write(*,*) 'dest_mesh_head: ',  trim(dest_mesh_head)
        write(*,*) 'table_file_head: ', trim(table_file_head)
      end if
!
      end subroutine set_control_4_dist_itp
!
!  ---------------------------------------------------------------------
!
      end program distribute_para_itp_table
