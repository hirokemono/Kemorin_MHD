!
      program distribute_para_itp_table
!
      use t_mesh_data
      use t_interpolate_table
!
      use m_ctl_data_gen_table
      use m_read_mesh_data
      use m_work_ditribute_itp
      use m_interpolate_table_IO
      use distribute_itp_tbl_4_para
      use set_parallel_mesh_in_1pe
      use itp_table_IO_select_4_zlib
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
      mesh1_file%file_prefix = dest_mesh_head
      allocate( femmesh_dest(nprocs_dest) )
      call s_set_parallel_mesh_in_1pe                                   &
     &   (mesh1_file, nprocs_dest, femmesh_dest)
!
      mesh1_file%file_prefix = org_mesh_head
      allocate( femmesh_org(nprocs_org) )
      call s_set_parallel_mesh_in_1pe                                   &
     &   (mesh1_file, nprocs_org, femmesh_org)
!
!
      table_file_header = sgl_table_file_head
      call load_interpolate_table(izero, single_tbl)
      write(*,*) 'interplation table is loaded'
!
!
!
      call set_work_4_ditribute_itp(nprocs_org, nprocs_dest,            &
     &    femmesh_org, femmesh_dest, single_tbl)
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
        call output_interpolate_table(my_rank, para_tbl(ip))
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
      call set_control_mesh_def(mesh1_file)
!
      org_mesh_head = mesh1_file%file_prefix
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
