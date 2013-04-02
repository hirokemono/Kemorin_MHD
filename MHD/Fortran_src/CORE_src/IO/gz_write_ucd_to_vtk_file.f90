!gz_write_ucd_to_vtk_file.f90
!      module gz_write_ucd_to_vtk_file
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on March, 2013
!
!      subroutine write_gz_parallel_vtk_file(my_rank, nprocs, istep)
!
!      subroutine write_ucd_data_2_gz_vtk(my_rank, istep)
!      subroutine write_ucd_data_2_gz_vtk_phys(my_rank, istep)
!      subroutine write_ucd_data_2_gz_vtk_grid(my_rank)
!
      module gz_write_ucd_to_vtk_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_ucd_data
      use m_field_file_format
      use set_ucd_file_names
      use skip_gz_comment
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_parallel_vtk_file(my_rank, nprocs, istep)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      integer(kind=kint), intent(in) :: my_rank, nprocs, istep
!
      character(len=kchara)  :: gzip_name, file_name, fname_tmp
      integer(kind = kint) :: ip
!
!
      if(my_rank .gt. 0) return
!
      call add_int_suffix(istep, ucd_header_name, fname_tmp)
      call add_pvtk_extension(fname_tmp, file_name)
      call add_gzip_extension(file_name, gzip_name)
!
      write(*,*) 'Write gzipped parallel VTK file: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      write(textbuf,'(a,a1)') '<File version="pvtk-1.0"', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &     '       dataType="vtkUnstructuredGrid"', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,i6,a,a1)')                                      &
     &     '       numberOfPieces="', nprocs, '" >', char(0)
      call write_compress_txt(nbuf, textbuf)
      do ip = 0, nprocs-1
        call set_parallel_ucd_file_name(ucd_header_name, iflag_vtk,     &
     &      ip, istep, file_name)
        write(textbuf,'(3a,a1)') '   <Piece fileName="',                &
     &                       trim(file_name), '" />', char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
      write(textbuf,'(a,a1)') '</File>', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call close_gzfile
!
      end subroutine write_gz_parallel_vtk_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_data_2_gz_vtk(my_rank, istep)
!
      use gz_vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_vtk_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &    'Write gzipped VTK data: ', trim(gzip_name)
!
      call write_gz_vtk_file(gzip_name,                                 &
     &    nnod_ucd, nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd,           &
     &    num_field_ucd, ntot_comp_ucd,  num_comp_ucd, phys_name_ucd,   &
     &    d_nod_ucd)
!
      end subroutine write_ucd_data_2_gz_vtk
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_data_2_gz_vtk_phys(my_rank, istep)
!
      use gz_vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_vtd_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &    'Write gzipped VTK field: ', trim(gzip_name)
!
      call write_gz_vtk_phys(gzip_name, nnod_ucd, num_field_ucd,        &
     &    ntot_comp_ucd, num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      end subroutine write_ucd_data_2_gz_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_data_2_gz_vtk_grid(my_rank)
!
      use gz_vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_grd_file_name(ucd_header_name, iflag_vtd_gz,    &
     &    my_rank, gzip_name)
!
      if(i_debug.gt.0 .or. my_rank.eq.0) write(*,*)                     &
     &    'Write gzipped VTK grid: ', trim(gzip_name)
!
      call write_gz_vtk_grid(gzip_name,                                 &
     &    nnod_ucd, nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd)
!
      end subroutine write_ucd_data_2_gz_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module gz_write_ucd_to_vtk_file
