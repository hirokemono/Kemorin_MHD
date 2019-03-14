!
      program distribute_para_itp_table
!
      use t_mesh_data
      use t_interpolate_table
      use t_file_IO_parameter
      use t_ctl_data_gen_table
      use t_work_ditribute_itp
!
      use m_interpolate_table_IO
      use distribute_itp_tbl_4_para
      use set_parallel_mesh_in_1pe
      use itp_table_IO_select_4_zlib
!
      implicit  none
!
      type(ctl_data_gen_table), save :: gtbl_ctl1
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
      type(work_ditribute_itp), save :: wk_dist_itp1
!
!  .......................
!
     integer :: ip, id_rank
!
!
      call read_control_4_distribute_itp(gtbl_ctl1)
      call set_control_4_dist_itp(gtbl_ctl1)
!
      allocate( femmesh_dest(nprocs_dest) )
      call s_set_parallel_mesh_in_1pe                                   &
     &   (dest_mesh_file, int(nprocs_dest), femmesh_dest)
!
      allocate( femmesh_org(nprocs_org) )
      call s_set_parallel_mesh_in_1pe                                   &
     &   (org_mesh_file, int(nprocs_org), femmesh_org)
!
!
      table_file_header = sgl_table_file_head
      call load_interpolate_table(0, single_tbl)
      write(*,*) 'interplation table is loaded'
!
!
!
      call set_work_4_ditribute_itp(nprocs_org, nprocs_dest,            &
     &    femmesh_org, femmesh_dest, single_tbl, wk_dist_itp1)
!
      nprocs_table = max(wk_dist_itp1%nprocs_itp_org,                   &
     &                   wk_dist_itp1%nprocs_itp_dest)
      allocate( para_tbl(nprocs_table) )
!
      call const_parallel_itp_tbl(single_tbl, wk_dist_itp1,             &
     &    nprocs_table, para_tbl)
!
      call dealloc_work_ditribute_itp(wk_dist_itp1)
!
      table_file_header = table_file_head
      write(*,*) 'table field header: ', trim(table_file_header)
      do ip = 1, nprocs_table
        id_rank = ip - 1
        call output_interpolate_table(id_rank, para_tbl(ip))
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
      subroutine set_control_4_dist_itp(gtbl_ctl)
!
      use t_ctl_data_gen_table
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_data
!
      type(ctl_data_gen_table), intent(in) :: gtbl_ctl
!
      call turn_off_debug_flag_by_ctl(id_rank, gtbl_ctl%src_plt)
      call set_control_smp_def(id_rank, gtbl_ctl%src_plt)
      call set_control_mesh_def(gtbl_ctl%src_plt, org_mesh_file)
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, gtbl_ctl%dst_plt, dest_mesh_file)
!
      nprocs_org = 1
      if(gtbl_ctl%src_plt%ndomain_ctl%iflag .gt. 0) then
        nprocs_org = gtbl_ctl%src_plt%ndomain_ctl%intvalue
      end if
!
      nprocs_dest = 1
      if(gtbl_ctl%dst_plt%ndomain_ctl%iflag .gt. 0) then
         nprocs_dest = gtbl_ctl%dst_plt%ndomain_ctl%intvalue
      end if
!
      if (gtbl_ctl%table_head_ctl%iflag .ne. 0) then
        table_file_head = gtbl_ctl%table_head_ctl%charavalue
      end if
!
      if (gtbl_ctl%single_itp_tbl_head_ctl%iflag .ne. 0) then
        sgl_table_file_head                                             &
     &     = gtbl_ctl%single_itp_tbl_head_ctl%charavalue
      end if
!
      ifmt_itp_table_file                                               &
     &   = choose_file_format(gtbl_ctl%fmt_itp_table_file_ctl)
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
