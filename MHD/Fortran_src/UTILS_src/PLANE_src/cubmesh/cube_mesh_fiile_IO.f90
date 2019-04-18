!
      module cube_mesh_fiile_IO
!
      use m_precision
!
      use m_filter_file_names
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine open_mesh_file                                         &
     &         (mesh_file_prefix, id_file, id_rank, c_size)
!
      use t_size_of_cube
      use set_mesh_file_names
!
      character(len=kchara), intent(in) :: mesh_file_prefix
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_file
      type(size_of_cube), intent(in) :: c_size
!
      character(len=kchara) ::  fname
!
! ***** open output file
!
      fname = set_mesh_file_name(mesh_file_prefix, id_ascii_file_fmt,   &
     &                           id_rank)
      open (id_file, file=fname, form='formatted' )
!
! ..... write 0. header lines
!
      write(id_file,'(a,i4,2(a,i4),a,i3,2(a,i3),a)')                    &
     &   '! mesh data for unit cube model, n_all=(',                    &
     &    c_size%nx_all,',',c_size%ny_all,',',c_size%nz_all, '), nd=(', &
     &    c_size%ndx   ,',', c_size%ndy   ,',', c_size%ndz,')'
!
       end subroutine open_mesh_file
!
! ----------------------------------------------------------------------
!
      end module cube_mesh_fiile_IO
