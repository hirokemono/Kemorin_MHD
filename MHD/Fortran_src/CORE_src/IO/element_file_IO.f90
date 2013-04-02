!element_file_IO.f90
!      module element_file_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine set_element_fname(my_rank)
!
!      subroutine input_element_comm_table(my_rank)
!      subroutine input_element_geometries(my_rank)
!
!      subroutine output_ele_comm_table(my_rank)
!      subroutine output_element_file(my_rank)
!      subroutine output_element_sph_file(my_rank)
!      subroutine output_element_cyl_file(my_rank)
!
!      subroutine input_element_comm_table_b(my_rank)
!      subroutine input_element_geometries_b(my_rank)
!
!      subroutine output_ele_comm_table_b(my_rank)
!      subroutine output_element_file_b(my_rank)
!      subroutine output_element_sph_file_b(my_rank)
!      subroutine output_element_cyl_file_b(my_rank)
!
      module element_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use set_parallel_file_name
      use element_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_element_fname(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      if(iflag_mesh_file_ext.gt.0) then
        call add_int_suffix(my_rank, mesh_ele_file_head, fname_tmp)
        call add_gfm_extension(fname_tmp, mesh_file_name)
      else
        call add_int_suffix(my_rank, mesh_ele_file_head,                &
     &      mesh_file_name)
      end if
!
      end subroutine set_element_fname
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine input_element_comm_table_b(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read binary element comm file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
      call read_element_comm_table_b
      close(input_file_code)
!
      end subroutine input_element_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine input_element_geometries_b(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read binary element comm file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
      call read_element_comm_table_b
      call read_element_geometries_b
      close(input_file_code)
!
      end subroutine input_element_geometries_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_ele_comm_table_b(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write binary element comm file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
      call write_element_comm_table_b
      close(input_file_code)
!
      end subroutine output_ele_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine output_element_file_b(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
        'Write binary element comm file: ', trim(mesh_file_name)
!
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'unformatted')
      call write_element_comm_table_b
      call write_element_geometry_b
!
      end subroutine output_element_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine input_element_comm_table(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_element_comm_table
      close(input_file_code)
!
      end subroutine input_element_comm_table
!
!------------------------------------------------------------------
!
      subroutine input_element_geometries(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_element_comm_table
      call read_element_geometries
      close(input_file_code)
!
      end subroutine input_element_geometries
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_ele_comm_table(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call write_element_comm_table
      close(input_file_code)
!
      end subroutine output_ele_comm_table
!
!------------------------------------------------------------------
!
      subroutine output_element_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call write_element_comm_table
      call write_element_geometry
      close(input_file_code)
!
      end subroutine output_element_file
!
!------------------------------------------------------------------
!
      subroutine output_element_sph_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call write_element_comm_table
      call write_element_geometry_sph
      close(input_file_code)
!
      end subroutine output_element_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_element_cyl_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call write_element_comm_table
      call write_element_geometry_cyl
      close(input_file_code)
!
      end subroutine output_element_cyl_file
!
!------------------------------------------------------------------
!
      end module element_file_IO
