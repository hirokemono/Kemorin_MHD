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
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 1.parallel information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_domain_info_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &      '! 2.mesh information (nodes and elements in partition)',   &
     &      char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 2.1 node (position) ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_geometry_info_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 2.2 element (connection) ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_element_info_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &         '! 3.import / export information ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 3.1 import ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_import_data_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 3.2 export ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_export_data_gz
!
      end subroutine write_geometry_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 1.parallel information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_domain_info_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &      '! 2.mesh information (nodes and elements in partition)',   &
     &      char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 2.1 node (position) ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_geometry_info_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &         '! 3.import / export information ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 3.1 import ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_import_data_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 3.2 export ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
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
!
      write(textbuf,'(a,a1)') '!' , char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  node position ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  by spherical coordinate', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!' , char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 1.parallel information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      call write_domain_info_gz
!
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &      '! 2.mesh information (nodes and elements in partition)',   &
     &        char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 2.1 node (r, theta, phi) ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_geometry_info_gz
!
      end subroutine output_node_sph_geometry_gz
!
!  ---------------------------------------------------------------------
!
      subroutine output_node_cyl_geometry_gz
!
!
      write(textbuf,'(a,a1)') '!' , char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  node position ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  by cylindrical coordinate', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!' , char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 1.parallel information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      call write_domain_info_gz
!
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &      '! 2.mesh information (nodes and elements in partition)',   &
     &      char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 2.1 node (s, phi, z) ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! ', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_geometry_info_gz
!
      end subroutine output_node_cyl_geometry_gz
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO
