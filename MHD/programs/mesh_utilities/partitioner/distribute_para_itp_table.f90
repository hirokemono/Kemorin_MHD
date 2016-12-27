!
      program distribute_para_itp_table
!
      use t_mesh_data
      use t_interpolate_table
      use t_file_IO_parameter
!
      use m_ctl_data_gen_table
      use m_work_ditribute_itp
      use m_interpolate_table_IO
      use distribute_itp_tbl_4_para
      use set_parallel_mesh_in_1pe
      use itp_table_IO_select_4_zlib
!
      implicit  none
!
      type(field_IO_params), save ::  org_mesh_file
      type(field_IO_params), save ::  dest_mesh_file
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
      allocate( femmesh_dest(nprocs_dest) )
      call s_set_parallel_mesh_in_1pe                                   &
     &   (dest_mesh_file, nprocs_dest, femmesh_dest)
!
      allocate( femmesh_org(nprocs_org) )
      call s_set_parallel_mesh_in_1pe                                   &
     &   (org_mesh_file, nprocs_org, femmesh_org)
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
      use m_file_format_switch
      use set_control_platform_data
      use set_ctl_params_2nd_files
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt1)
      call set_control_smp_def(my_rank, plt1)
      call set_control_mesh_def(plt1, org_mesh_file)
      call set_control_new_mesh_file_def(dest_mesh_file)
!
      nprocs_org = 1
      if(plt1%ndomain_ctl%iflag .gt. 0) then
        nprocs_org = plt1%ndomain_ctl%intvalue
      end if
!
      nprocs_dest = 1
      if(new_plt%ndomain_ctl%iflag .gt. 0) then
         nprocs_dest = new_plt%ndomain_ctl%intvalue
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
        write(*,*) 'table_file_head: ', trim(table_file_head)
      end if
!
      end subroutine set_control_4_dist_itp
!
!  ---------------------------------------------------------------------
!
      end program distribute_para_itp_table
