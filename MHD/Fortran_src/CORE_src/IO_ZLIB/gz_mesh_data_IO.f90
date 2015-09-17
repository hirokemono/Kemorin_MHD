!
!      module gz_mesh_data_IO
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine write_geometry_data_gz
!      subroutine write_filter_geometry_gz
!
!      subroutine read_geometry_data_gz
!      subroutine read_num_node_ele_gz
!      subroutine read_num_node_gz
!      subroutine read_filter_geometry_gz
!
!      subroutine output_node_sph_geometry_gz
!      subroutine output_node_cyl_geometry_gz
!
      module gz_mesh_data_IO
!
      use m_precision
!
      use m_read_mesh_data
      use skip_gz_comment
      use gz_domain_data_IO
      use gz_node_geometry_IO
      use gz_element_connect_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_data_gz
!
      use m_fem_mesh_labels
!
!
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_domain_info_gz
!
!
      textbuf = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_geometry_info_gz
!
!
      textbuf = hd_fem_elem() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_element_info_gz
!
!
      textbuf = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_import_data_gz
!
!
      textbuf = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_export_data_gz
!
      end subroutine write_geometry_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_gz
!
      use m_fem_mesh_labels
!
!
      textbuf = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_domain_info_gz
!
!
      textbuf = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_geometry_info_gz
!
!
      textbuf = hd_fem_import() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_import_data_gz
!
!
      textbuf = hd_fem_export() // char(0)
      call gz_write_textbuf_no_lf
!
      call write_export_data_gz
!
      end subroutine write_filter_geometry_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine read_geometry_data_gz
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz
!        write(*,*) 'read_geometry_info_gz'
        call read_geometry_info_gz
!
!  ----  read element data -------
!
!        write(*,*) 'read_number_of_element_gz'
        call read_number_of_element_gz
!        write(*,*) 'read_element_info_gz'
        call read_element_info_gz
!
! ----  import & export 
!
!        write(*,*) 'read_import_data_gz'
        call read_import_data_gz
!        write(*,*) 'read_export_data_gz'
        call read_export_data_gz
!
       end subroutine read_geometry_data_gz
!
!------------------------------------------------------------------
!
       subroutine read_num_node_ele_gz
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz
!        write(*,*) 'read_geometry_info_gz'
        call read_geometry_info_gz
!
!  ----  read element data -------
!
!        write(*,*) 'read_number_of_element_gz'
        call read_number_of_element_gz
!
       end subroutine read_num_node_ele_gz
!
!------------------------------------------------------------------
!
       subroutine read_num_node_gz
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz
!
       end subroutine read_num_node_gz
!
!------------------------------------------------------------------
!
       subroutine read_filter_geometry_gz
!
!
!        write(*,*) 'read_domain_info_gz'
        call read_domain_info_gz
!        write(*,*) 'read_number_of_node_gz'
        call read_number_of_node_gz
!        write(*,*) 'read_geometry_info_gz'
        call read_geometry_info_gz
!
! ----  import & export 
!
!        write(*,*) 'read_import_data_gz'
        call read_import_data_gz
!        write(*,*) 'read_export_data_gz'
        call read_export_data_gz
!
       end subroutine read_filter_geometry_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_node_sph_geometry_gz
!
      use m_fem_mesh_labels
!
!
      textbuf = hd_fem_para_sph() // hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz
!
      textbuf = hd_fem_node_sph() // char(0)
      call gz_write_textbuf_no_lf
      call write_geometry_info_gz
!
      end subroutine output_node_sph_geometry_gz
!
!  ---------------------------------------------------------------------
!
      subroutine output_node_cyl_geometry_gz
!
      use m_fem_mesh_labels
!
!
      textbuf = hd_fem_para_cyl() // hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz
!
      textbuf = hd_fem_node_cyl() // char(0)
      call gz_write_textbuf_no_lf
      call write_geometry_info_gz
!
      end subroutine output_node_cyl_geometry_gz
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO
