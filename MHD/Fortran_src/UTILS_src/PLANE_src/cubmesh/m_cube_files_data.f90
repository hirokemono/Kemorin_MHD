!
      module m_cube_files_data
!
      use m_precision
!
      use m_filter_file_names
!
      implicit none
!
!  * parameters for i/o code
!      l_out      : file code for output
!      l_err      : file code for standard error
      integer(kind=kint ), parameter  ::  l_in  =  9
      integer(kind=kint ), parameter  ::  l_out = 10
      integer(kind=kint ), parameter  ::  nb_out = filter_file_code
!
      character(len= 4 )   ::  penum, penum_left
      character(len=kchara) ::  fname
      character(len=kchara) :: nb_name
!
      character(len=kchara) :: mesh_file_header
      character(len=kchara) :: filter_file_header
      character(len=kchara) :: z_filter_header
!
      character(len=kchara) :: filter_edge_header = 'filter_edge_l'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine open_mesh_file(id_rank, c_size)
!
      use t_size_of_cube
      use set_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(size_of_cube), intent(in) :: c_size
!
! ***** open output file
!
      fname = set_mesh_file_name(mesh_file_header, id_ascii_file_fmt,   &
     &                           id_rank)
      open (l_out, file=fname, form='formatted' )
!
! ..... write 0. header lines
!
      write(l_out,'(a,i4,2(a,i4),a,i3,2(a,i3),a)')                      &
     &   '! mesh data for unit cube model, n_all=(',                    &
     &    c_size%nx_all,',',c_size%ny_all,',',c_size%nz_all, '), nd=(', &
     &    c_size%ndx   ,',', c_size%ndy   ,',', c_size%ndz,')'
!
       end subroutine open_mesh_file
!
! ----------------------------------------------------------------------
!
      end module m_cube_files_data
