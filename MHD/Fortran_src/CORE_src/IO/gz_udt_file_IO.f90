!
!      module gz_udt_file_IO
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine write_gz_ucd_file(my_rank, istep)
!      subroutine write_gz_udt_file(my_rank, istep)
!      subroutine write_gz_grd_file(my_rank)
!
!      subroutine read_udt_file_gz(my_rank, istep)
!      subroutine read_and_allocate_udt_head_gz(my_rank, istep)
!      subroutine read_and_allocate_udt_file_gz(my_rank, istep)
!
!      subroutine write_gz_grd_file
!
      module gz_udt_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_ucd_data
      use m_field_file_format
      use set_ucd_file_names
      use skip_gz_comment
      use gz_ucd_data_IO
!
      implicit none
!
      private :: write_gz_udt_fields, write_gz_ucd_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_file(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_ucd_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &      'Write gzipped ucd file: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_ucd_mesh
      call write_gz_udt_fields
      call close_gzfile
!
      end subroutine write_gz_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_udt_file(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &      'Write gzipped ucd file: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_udt_fields
      call close_gzfile
!
      end subroutine write_gz_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_grd_file(my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_grd_file_name(ucd_header_name, iflag_udt_gz,    &
     &    my_rank, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &      'Write gzipped ucd grid file: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_ucd_mesh
      call close_gzfile
!
      end subroutine write_gz_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_udt_file_gz(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_udt_field_header(num_field_ucd, num_comp_ucd,        &
     &    phys_name_ucd)
!
      call cal_istack_ucd_component
!
      call read_gz_single_udt_data(nnod_ucd, ntot_comp_ucd, d_nod_ucd)
!
      call close_gzfile
!
      end subroutine read_udt_file_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_and_allocate_udt_head_gz(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &     'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_udt_field_num(num_field_ucd)
      call allocate_ucd_phys_name
!
      call read_gz_udt_field_name(num_field_ucd, num_comp_ucd,          &
     &    phys_name_ucd)
!
      call close_gzfile
!
      call cal_istack_ucd_component
      call allocate_ucd_phys_data
!
      end subroutine read_and_allocate_udt_head_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_and_allocate_udt_file_gz(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_udt_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &     'Write gzipped data file: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_udt_field_num(num_field_ucd)
      call allocate_ucd_phys_name
!
      call read_gz_udt_field_name(num_field_ucd, num_comp_ucd,          &
     &    phys_name_ucd)
!
      call cal_istack_ucd_component
      call allocate_ucd_phys_data
!
      call read_gz_single_udt_data(nnod_ucd, ntot_comp_ucd, d_nod_ucd)
      call close_gzfile
!
      end subroutine read_and_allocate_udt_file_gz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_udt_fields
!
!
      if(num_field_ucd .gt. 0) then
        call write_gz_udt_field_header(num_field_ucd,                   &
     &      num_comp_ucd, phys_name_ucd)
        call write_gz_single_udt_data(nnod_ucd, ntot_comp_ucd,          &
     &      inod_gl_ucd, d_nod_ucd)
      end if
!
      end subroutine write_gz_udt_fields
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_ucd_mesh
!
!
      call write_gz_udt_mesh_header(nnod_ucd, nele_ucd, ntot_comp_ucd)
!
      call write_gz_single_udt_data(nnod_ucd, ithree,                   &
     &    inod_gl_ucd, xx_ucd)
      call write_gz_single_grd_connect(nnod_4_ele_ucd, nele_ucd,        &
     &    iele_gl_ucd, ie_ucd)
!
      end subroutine write_gz_ucd_mesh
!
! -----------------------------------------------------------------------
!
      end module gz_udt_file_IO
