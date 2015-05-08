/* #include "/usr/cvs/ciaosystems/CiaoDE/ciao/include/LINUXi86/ciao_prolog.h" */
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
void socket_c(int puerto,int * number_out) {
    struct sockaddr_in sa;
    if((* number_out = socket(AF_INET, SOCK_DGRAM,IPPROTO_UDP))<0)
	printf("Error en creación de socket udp\n");
    sa.sin_port = htons(puerto);
    sa.sin_family = AF_INET;
    sa.sin_addr.s_addr = INADDR_ANY;
    if(bind(* number_out,(struct sockaddr *) &sa, sizeof(sa))<0)
	printf("Error en bind socket udp\n");
    //printf("nro socket %d , salida bind %d,puerto %d \n", * number_out, b,puerto);
}

char buff[2049];

char *  recv_c(int socket,int largo){
    struct sockaddr_in sa;
    //int size = sizeof(sa);
    int largoreal=recvfrom(socket,buff,largo,0,NULL,NULL);//(struct sockaddr *) &sa,(int *) sizeof(sa));
    if (largoreal<0)
	printf("Error en recvfrom socket udp, largo real %d\n",largoreal);
    //printf("buffer en c %s \n",buff);
    buff[largoreal]='\0';
    return (char *) buff; 
}

void close_c(int socket){
    if (close(socket)<0)
	printf("Error en close socket udp\n");
}
