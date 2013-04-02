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
      integer (kind = kint) :: iflag_data_f = 0
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine open_mesh_file(my_rank)
!
      use m_size_4_plane
      use m_size_of_cube
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank
!
! ***** open output file
!
             call add_int_suffix(my_rank, mesh_file_header, fname)
             open (l_out, file=fname, form='formatted' )
!
! ..... write 0. header lines
!
             write(l_out,'(a,i4,2(a,i4),a,i3,2(a,i3),a)')               &
     &         '! mesh data for unit cube model, n_all=(',              &
     &          nx_all,',',ny_all,',',nz_all,'), nd=(',                 &
     &          ndx   ,',',ndy   ,',',ndz   ,')'
!
       end subroutine open_mesh_file
!
! ----------------------------------------------------------------------
!
      subroutine open_mesh_file_b(my_rank)
!
      use m_size_of_cube
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank
!
! ***** open output file
!
             call add_int_suffix(my_rank, mesh_file_header, fname)
             open (l_out, file=fname, form='unformatted' )
!
! ..... write 0. header lines
!
!             write(l_out)                                              &
!     &         '! mesh data for unit cube model, n_all=(',             &
!     &          nx_all,',',ny_all,',',nz_all,'), nd=(',                &
!     &          ndx   ,',',ndy   ,',',ndz   ,')'
!
       end subroutine open_mesh_file_b
!
! ----------------------------------------------------------------------
!
     end module m_cube_files_data

